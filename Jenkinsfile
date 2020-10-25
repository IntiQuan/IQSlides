pipeline {
  agent any
  stages {

    stage('get') {
      steps {
        git(url: 'https://github.com/IntiQuan/IQSlides', branch: 'master')
		sh 'git checkout master'
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

    stage('clean') {
      steps {
        sh 'rm -r /tmp/R/IQSlides'
      }
    }
	
    stage('commitlog') {
      steps {
        sh 'git add .'
		sh 'git commit -m "jenkins update"'
		sh 'git push --set-upstream origin master'
      }
    }


  }
}
