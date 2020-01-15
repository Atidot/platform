# This can be used as a decorator that appears in the Python AST
# without affecting how a program runs. This is essentially a way
# of annotating the entrypoint of a model that can be run on a dataframe.
def entrypoint(func):
    return func
