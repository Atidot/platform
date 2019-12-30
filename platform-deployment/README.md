# platform-deployment

## intro

allows us to declare a deployment inside an aws instance, with dockers, disks and secrets declarative management

## example script

``` haskell
nsss :: DeploymentM Bool
nsss = do
    secret <- secret "tutorials/MyFirstTutorialSecret"
    dir <- mount "data"
    b <- container "hello-world"
    attachSecret secret "hello-world"
    attachVolume dir "hello-world"
    execute [] "hello-world" []
    return b
```

This script Declares:

1. A secret
2. An external disk "data"
3. a container named hello world
4. attaches the secret to the container
5. attaches the mount to the container
6. executes the container

## How to Run

``` bash
$ cd ../build
..
$ make shell
... # in the newly opened nix shell
$ platform-deployment-exe --script nsss
{initializes terraform and generates files from example}
$ cd terraform_dep
...
$ ls
aws_cli_config.tf  example.tf
$ terraform apply # applies genetated configuration
...
```
