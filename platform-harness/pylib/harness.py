import os

def platformCheck():
    s = os.environ.getenv('PLATFORM_HARNESS', '')
    if s == '1':
        return True
    else:
        return False

# The harness module provides environment variables and queues
def consumers():
    l = os.environ.getenv('ATIDOT_CONSUMERS', '')
    validateConsumers(l)
    consumerList = list()
    for elt in l:
        d = dict()
        pairs = elt.split(',')
        for pair in pairs:
            p = pair.split('=')
            key, val = p[0], p[1]
            d[key] = val
        consumerList.append(Entity(**d))
    return consumerList

def validateConsumers(l):
    pass

def producers():
    l = os.environ.getenv('ATIDOT_PRODUCERS', '')
    validateProducers(l)
    producerList = list()
    for elt in l:
        d = dict()
        pairs = elt.split(',')
        for pair in pairs:
            p = pair.split('=')
            key, val = p[0], p[1]
            d[key] = val
        producerList.append(Entity(**d))
    return producerList

def validateConsumers(l):
    pass
