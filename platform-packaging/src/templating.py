from jinja2 import Environment, PackageLoader, select_autoescape

env = Environment(
        loader=PackageLoader('platform-packaging', 'templates'),
        autoescape=select_autoescape(['py'])
)
