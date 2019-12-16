import os
import sys
{% block lib_imports %}

{% block model %}

def main():
    i = stdin.readlines()
    config = json.loads(i)

    input_df = {% block read_df %}{% endblock %}

    result = {% block run_model %}

    stdout.write(json.dumps(result))

    try:
        {% block write_result %}
    except AttributeError:
        pass
