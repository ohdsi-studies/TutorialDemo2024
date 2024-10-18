remotes::install_github('ohdsi/ProtocolGenerator')
ProtocolGenerator::generateProtocol(
  jsonLocation = '/Users/jreps/Documents/GitHub/TutorialDemo2024/analysisSpecifications.json',
  webAPI = 'https://atlas-demo.ohdsi.org/WebAPI',
  outputLocation = '/Users/jreps/Documents/TutorialDemo2024/protocol',
  outputName = 'protocol.html'
)
