let Prelude = ../../External/Prelude.dhall

let S = ../../Lib/SelectFiles.dhall
let Cmd = ../../Lib/Cmds.dhall

let Pipeline = ../../Pipeline/Dsl.dhall
let JobSpec = ../../Pipeline/JobSpec.dhall

let Command = ../../Command/Base.dhall
let Docker = ../../Command/Docker/Type.dhall
let Size = ../../Command/Size.dhall

let jobDocker = Cmd.Docker::{image = (../../Constants/ContainerImages.dhall).codaToolchain}

in

Pipeline.build
  Pipeline.Config::{
    spec = JobSpec::{
      dirtyWhen = [ S.contains "helm/", S.strictlyStart (S.contains "src/helm") ],
      path = "Lint",
      name = "HelmRelease"
    },
    steps = [
      Command.build
        Command.Config::{
          commands = [ Cmd.runInDocker jobDocker "buildkite/scripts/helm-lint.sh" ]
          , label = "Helm chart lint steps"
          , key = "lint-helm-chart"
          , target = Size.Medium
          , docker = None Docker.Type
        }
    ]
  }
