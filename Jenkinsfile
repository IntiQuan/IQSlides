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
        sh '''mkdir -p /tmp/R ;

R CMD INSTALL -l /tmp/R .












'''
      }
    }

    stage('message') {
      steps {
        sh 'ls -lia .'
      }
    }

    stage('test') {
      steps {
        sh '''echo -e "library(\\"IQSlides\\", lib.loc=\\"/tmp/R\\")\\ndevtools::test()\\n" > run.R;
Rscript run.R'''
      }
    }

  }
}