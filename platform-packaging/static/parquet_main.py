{% extends 'base_main.py' %}
{% block lib_imports %}import pandas as pd{% endblock %}
{% block read_df %}pd.read_parquet(config.input_path, engine='fastparquet'){% endblock %}
{% block write_result %}pass{% endblock %}
