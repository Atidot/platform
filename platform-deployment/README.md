# platform-deployment

## intro

allows us to declare a deployment inside an aws instance, with dockers, disks and secrets declarative management

Platform deployment package holds two parts:
1. Deployment-dsl - High language implemented with free monad, that describes the things we need in order to deploy software configuration (containers, secrets and data) (#########Link to Deployment.hs)
1. Interpreters for deployment dsl - there are two interpreters
1. 1. AMI - starts a machine in aws with terraform configuration, then apply changes, and eventually, saves an AMI containing all the changes supplied. This is dynamic, which means that the machine state is changed after applying each line (##### link to AMI)
1. 1. Terraform - Generates a terraform deployment file, that once applied, will start and then configure a full machine, on `terraform init`. This is static, which means that the interpreter is run, and then a file is generated. There is no AMI ready at the end of the interpreter run. It is more mature of the two. (Link to Terraform.hs)

## Terraform requirements and notes:

* Terraform (supply link) needs to be installed.
* In the terraform configuration, there is an ssh key path (Link in config). It is needed to be supplied for the remote provisioners and for ssh connection into the deployed machine
* AWS credentials for configuring the aws cli are needed but are prompted during `terraform init`
* AWS EBS volumes are needed in order for mounting to work properly
* Volume is needed to be created before running the interpreter.It’s (Volume) name/id should be hardcoded into the configuration. There is an example for that in the code
* Terraform deploys a single virtual machine, with all sorts of addons required for ssh into that machine. Then it uses a series of remote provisioners to change the state of the machine:
* 1. Installation of software
* 1. Mounting secrets initialization
* 1. Mounting and cli
* 1. Running dockers (Run command in the deployment DSL)
* In order to run new scripts, they should be added in Main.hs, similarly to the already existing scripts.
Nes,nesa,std,sat,cdne - are error checking scripts, they should fail
* All secrets used must be properly defined in advance in the AWS secrets manager


## Next Steps

* CLI Argument for terraform configurations (using json, from file?)
* In order to avoid credentials for aws-cli completely, we can register an IAM for the AMI, or load the credentials by using user data
* Separate failed scripts to tests, to ensure that behavior is stable
* Instead of provisioner for running dockers, a provisioner for preparing a script that run dockers on computer start
* Better name giving to the ebs volumes (will be relevant if we want more than one volume)
* Improving the description of secrets in the dsl (they come in tuples at least, and are in a json format),
* Calling Container keyword should return the name of the container for use, instead of boolean right now (which holds no real meaning)

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
