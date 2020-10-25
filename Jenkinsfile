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
        sh 'R CMD INSTALL --no-multiarch --with-keep.source .'
      }
    }

    stage('message') {
      steps {
        sh 'ls -lia .'
      }
    }

    stage('test') {
      steps {
        sh 'Rscript "devtools::test()'
      }
    }
  }
}
