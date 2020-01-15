{% extends 'base_main.py' %}
{% block lib_imports %}import pandas as pd{% endblock %}
{% block read_df %}pd.read_csv(config.input_path){% endblock %}
{% block write_result %}pd.write_csv(config.output_path, result){% endblock %}
