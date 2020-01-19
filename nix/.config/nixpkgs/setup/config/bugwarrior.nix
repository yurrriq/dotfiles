{ pkgs, ... }:

{

  home.packages = with pkgs; [
    bugwarrior
    pass
  ];

  imports = [
    <setup/config/taskwarrior.nix>
  ];

  xdg.configFile.bugwarrior = {
    text = ''
      [general]
      targets = sportradar_jira
      # taskrc = /home/e.bailey/.taskrc
      inline_links = False
      annotation_links = True
      annotation_comments = True
      legacy_matching = False
      log.level = DEBUG
      log.file = /home/e.bailey/log/bugwarrior.log
      annotation_length = 80

      [sportradar_jira]
      service = jira
      jira.base_uri = https://jira.sportradar.ag
      jira.username = e.bailey
      jira.password = @oracle:eval:pass jira.sportradar.ag
      jira.query = assignee = e.bailey and resolution = unresolved
      jira.version = 8
      jira.add_tags = work
      jira.description_template = {{jiraid}}: {{jirasummary}}
    '';
  };

}
