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
        sh 'R CMD build .'
      }
    }

    stage('message') {
      steps {
        sh 'ls -lia .'
      }
    }

    
  }
}