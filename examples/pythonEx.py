#import alpha
#import beta
import requests
import pika #!platform pika
# Issues: 
# Perhaps there are multiple packages exporting alpha, in which the most recently-installed one wins out.
# It is difficult to tell which function calls refer to alpha as opposed to the other modules. 
# If both modules called "alpha" export the same names then we have another problem. 

def main():
    functionFromSomeModule() # Secretly, this comes from alpha.

# Other option: introspect the package downloads (in .whl and .tar.gz formats
