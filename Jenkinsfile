pipeline {
  agent any
  stages {
    stage('get') {
      steps {
        git(url: 'https://github.com/IntiQuan/IQSlides', branch: 'master')
      }
    }

    stage('build') {
      steps {
        sh 'mkdir -p /tmp/R'
        sh 'R CMD INSTALL -l /tmp/R .'
      }
    }

    stage('test') {
      steps {
        sh 'Rscript Jenkinstest.R'
      }
    }

  }
}