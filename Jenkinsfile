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
        sh 'R CMD INSTALL .'
      }
    }

    stage('test') {
      steps {
        sh 'Rscript Jenkinstest.R'
      }
    }

  }
}
