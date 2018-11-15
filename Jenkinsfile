#!groovy
// -*- mode: groovy -*-

def finalHook = {
  runStage('store CT logs') {
    archive '_build/test/logs/'
  }
}

build('cowboy_access_log', 'docker-host', finalHook) {
  checkoutRepo()
  loadBuildUtils()

  def pipeDefault
  def withWsCache
  runStage('load pipeline') {
    env.JENKINS_LIB = "build_utils/jenkins_lib"
    pipeDefault = load("${env.JENKINS_LIB}/pipeDefault.groovy")
    withWsCache = load("${env.JENKINS_LIB}/withWsCache.groovy")
  }

  pipeDefault() {

    if (!masterlikeBranch()) {

      runStage('compile') {
        withGithubPrivkey {
          sh 'make wc_compile'
        }
      }

      runStage('lint') {
        sh 'make wc_lint'
      }

      runStage('xref') {
        sh 'make wc_xref'
      }

      //runStage('dialyze') {
      //  withWsCache("_build/default/rebar3_19.1_plt") {
      //    sh 'make wc_dialyze'
      //  }
      //}

      runStage('test') {
        sh "make wc_test"
      }

    }

  }
}
