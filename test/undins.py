# -*- coding: utf-8 -*-
import json
import pandas as pd
import undins.model_undins as model_undins
import undins.model_score as model_score
import undins.logger as logger
import argparse
import os
import urllib
from munch import munchify
from syslog import openlog
from enrichment.zipcode import zip_fill, abbreviate_state, geographic_fallbacks
from undins.need import need_analysis, NEED_C
from sqlalchemy import create_engine #!platform SQLAlchemy
from collections.abc import Iterator
import psycopg2

"""Main module."""


LAST_MODEL_PATH = "/data/last_model"


class DatabaseConnection:
    def __init__(self, config):
        path = os.path.join(config.password.dir, config.password.file)
        with open(path) as f:
            self.password = f.read().rstrip()
        self.dbconn = None
        self.config = config


class MySQL(DatabaseConnection):
    def __init__(self, config):
        DatabaseConnection.__init__(self, config)

    def __enter__(self):
        config = self.config
        self.dbconn = create_engine(
            'mysql+pymysql://'
            f'{config.user}:{self.password}'
            '@'
            f'{config.host}/{config.dbname}'
        )
        return self.dbconn

    def __exit__(self, exc_type, exc_val, exc_tb):
        return  # Does nothing since sqlalchemy's engine handles the connection


class PostgresConnection(DatabaseConnection):
    def __init__(self, config):
        DatabaseConnection.__init__(self, config)

    def __enter__(self):
        config = self.config
        self.dbconn = create_engine(
            'postgresql+psycopg2://'
            f'{config.user}:{self.password}'
            '@'
            f'{config.host}/{config.dbname}'
        )
        return self.dbconn

    def __exit__(self, exc_type, exc_val, exc_tb):
        return  # Does nothing since sqlalchemy's engine handles the connection


class MSSQLConnection(DatabaseConnection):
    def __init__(self, config):
        DatabaseConnection.__init__(self, config)

    def __enter__(self):
        config = self.config
        driver_path = os.environ["MSODBCSQL17Driver"]

        conn_string = (
            f'DRIVER={driver_path};'
            f'SERVER={config.host};'
            f'DATABASE={config.dbname};'
            f'UID={config.user};'
            f'PWD={self.password}'
        )

        self.dbconn = create_engine(
            "mssql+pyodbc:///?odbc_connect={}".format(
                urllib.parse.quote_plus(conn_string)
            )
        )

        return self.dbconn

    def __exit__(self, exc_type, exc_val, exc_tb):
        return  # Does nothing since sqlalchemy's engine handles the connection


def make_connection(conn_type, conn_config):
    if conn_type == "postgres":
        return PostgresConnection(conn_config)

    elif conn_type == "mssql":
        return MSSQLConnection(conn_config)

    if conn_type == "mysql":
        return MySQL(conn_config)

    else:
        raise Exception(
            "Unsupported database type {}".format(conn_config)
        )


@logger.calculate_time
def db_read(db_config, input_selector, chunksize=None):
    if db_config.type == "csv":
        return pd.read_csv(input_selector)
    with make_connection(db_config.type, db_config.conn) as conn:
        return pd.read_sql_query(input_selector, conn, chunksize=chunksize)


@logger.calculate_time
def db_write(db_config, output_selector, dataframe):
    if db_config.type == "csv":
        dataframe.to_csv(output_selector.table, index=False)

    else:
        with make_connection(db_config.type, db_config.conn) as conn:
            dataframe.to_sql(
                name=output_selector.table,
                schema=output_selector.schema,
                con=conn,
                if_exists='append'
            )


@logger.calculate_time
@logger.df_shape_in_and_out
def train_or_predict(enriched_df, config, model_dir, model_id, should_train=True):

    prediction_df = pd.DataFrame()
    for name, products in [
        ('term', ['Term Life']),
        ('universal_whole', ['Universal Life', 'Whole Life'])
    ]:
        input_df = enriched_df[
            
            enriched_df[config.columns.client.product].isin(products)]
        if len(input_df)>0:
            prediction_df = prediction_df.append(run_model(
                config,
                os.path.join(model_dir, name),
                model_id,
                input_df,
                should_train
            ))

    output_columns = (config.model.id_cs +
                      [NEED_C, model_score.UI, model_score.sell_l] +
                      config.model.zipcode_numerical_cs)
    output_df = prediction_df[output_columns].set_index("ID")

    db_write(
        db_config=config.locations.result,
        output_selector=config.locations.result.outputs.default,
        dataframe=output_df)


@logger.calculate_time
@logger.df_shape_in_and_out
def arrange(df, config):

    df, zip_c, zip_errs = zip_fill(
        df, config.model.id_cs[0], config.columns.client.zipcode)
    if zip_errs:
        logger.log_err('zip errors: ', zip_errs)

    # add need data
    need_df = db_read(
        config.locations.external,
        config.locations.external.inputs.need
    )
    df = need_analysis(
        need_df=need_df,
        df=df,
        faceamount=config.model.target_c,
        zipcode=zip_c,
        gender=config.columns.client.gender,
        age=config.columns.client.age,
        birthdate_year=config.model.birthdate_year_c
    )

    # add zip data
    zip_df, city_df, state_df, state_name_df = \
        (db_read(
            config.locations.external,
            config.locations.external.inputs[x]
        ) for x in
            ("zipcode", "city", "state", "state_name")
        )

    df, state_col = abbreviate_state(
        data=df,
        state_col=config.columns.client.state,
        data_external=state_name_df
    )

    df, signed_state_col = abbreviate_state(
        data=df,
        state_col=config.columns.client.signed_state,
        data_external=state_name_df
    )

    enriched_df = geographic_fallbacks(
        df=df,
        zip_col_client=zip_c,
        city_col_client=config.columns.client.city,
        state_col_client=state_col,
        signed_state_col_client=signed_state_col,
        zip_db=zip_df,
        city_db=city_df,
        state_db=state_df
    )

    logger.log_debug(enriched_df.columns.tolist())

    return enriched_df

@logger.calculate_time
def arrange_and_predict(config, model_dir, model_id, should_train=True, chunksize=None):

    if should_train:
        df = db_read(config.locations.client,
                     config.locations.client.inputs.train_predict_query)
        enriched_df = arrange(df, config)
        train_or_predict(enriched_df, config, model_dir, model_id, should_train)

    else:
        db_read_return = db_read(config.locations.client,
                             config.locations.client.inputs.predict_query,
                             chunksize)
        if isinstance(db_read_return, Iterator):
            for df in db_read_return:
                enriched_df = arrange(df, config)
                train_or_predict(enriched_df, config, model_dir, model_id, should_train)
        else:
            enriched_df = arrange(db_read_return, config)
            train_or_predict(enriched_df, config, model_dir, model_id, should_train)



@logger.calculate_time
@logger.df_shape_in_and_out
def run_model(config, model_dir, model_id, input_df, should_train):
    if should_train:
        model_ui = model_undins.ModelUndins(
            id_cs=config.model.id_cs, target_c=config.model.target_c,
            categorical_cs=config.model.categorical_cs,
            numerical_cs=config.model.numeric_cs,
            zipcode_numerical_cs=config.model.zipcode_numerical_cs,
            age_issue_c=config.model.age_issue_c,
            dob_year_c=config.model.birthdate_year_c,
            date_cs=config.model.date_cs,
            info_cs=config.model.info_cs + [NEED_C],
            chunks_size=1_000_000,
            n_jobs_transform=1
        )
        prediction_df = model_ui.fit_predict(input_df)
        model_ui.save(model_dir)
        with open(LAST_MODEL_PATH, 'w') as lmf:
            lmf.write(model_id)
        return prediction_df
    else:
        model_ui = model_undins.ModelUndins.load(model_dir)
        return model_ui.predict(input_df)

"""
cat ~/Downloads/k8s-deploy-prod.json | jq -c '.data."config.json"' | head -5 | tail -1  | sed 's|\\"|"|g' | sed -E 's|^.(.+)|\1|g' | sed -E 's|^(.+).|\1|g' | jq . > ~/Downloads/config.example.json
"""
def main():
    openlog('undins')
    dynamicConfig = munchify(json.loads(os.environ['CONFIG']))
    if "jobId" in dynamicConfig:
        openlog('undins-' + dynamicConfig.jobId[:8])
    logger.log_debug(dynamicConfig)

    parser = argparse.ArgumentParser(
        description='Project under-insurance values')
    parser.add_argument(
        '--config-file',
        '-c',
        default='/data/config/config.json',
        type=str,
        required=False,
        help="location of the configuration file",
        dest="config",
        nargs="?"
    )

    args = parser.parse_args()

    with open(args.config) as json_data:
        config = munchify(json.load(json_data))
        logger.log_debug(config)

    should_train = {
        'TrainPredict': True,
        'Predict': False
    }[dynamicConfig.jobType]
    if should_train:
        model_id = dynamicConfig.jobId
    else:
        with open(LAST_MODEL_PATH, 'r') as lmf:
            model_id = lmf.read()
    model_dir = os.path.join('/data/', model_id)

    if not os.path.exists(model_dir):
        os.mkdir(model_dir)

    status = logger.Status(config.mq, config.statusQ, dynamicConfig.jobId)

    try:
        status.send("Started")
        chunksize = config.model.get("chunksize")
        arrange_and_predict(config, model_dir, model_id, should_train,chunksize)
        status.send("Finished")
    except Exception:
        status.send("Failed")
        raise


if __name__ == "__main__":
    try:
        main()
    except Exception:
        logger.log_exception()
        raise
