module Spec.TestScripts where

import "platform-deployment" Atidot.Platform.Deployment

hello :: DeploymentM ()
hello = do
    _ <- container placeHolderContainer
    return ()

noneExistentSecret :: DeploymentM ()
noneExistentSecret = do
    let noneExistantSecretPlaceholder = "some/secret"
    _ <- secret noneExistantSecretPlaceholder
    return ()

noneExistentSecretAttached :: DeploymentM ()
noneExistentSecretAttached = do
    let noneExistantSecretPlaceholder = "some/secret"
    attachSecret noneExistantSecretPlaceholder placeHolderContainer
    return ()

secretDeclaredTwice :: DeploymentM ()
secretDeclaredTwice = do
    _ <- secret placeHolderSecret
    _ <- secret placeHolderSecret
    return ()

secretAttachedTwice :: DeploymentM ()
secretAttachedTwice = do
    s <- secret placeHolderSecret
    c <- container placeHolderContainer
    attachSecret s c
    attachSecret s c

containerDoesNotExists :: DeploymentM ()
containerDoesNotExists = do
    s <- secret placeHolderSecret
    attachSecret s placeHolderContainer
