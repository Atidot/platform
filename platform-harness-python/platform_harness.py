import os

def amqpURL():
    return os.environ.getenv('PLATFORM_AMQP_URL', '')

def platformCheck():
    s = os.environ.getenv('PLATFORM_HARNESS', '')
    if s == '1':
        return True
    else:
        return False

# The harness module provides environment variables and queues
def consumers():
    return getPairs('PLATFORM_CONSUMERS')

def producers():
    return getPairs('PLATFORM_PRODUCERS')

def getPairs(envVar):
    l = os.environ.getenv(envVar, '')
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
