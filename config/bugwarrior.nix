{ config, pkgs, ... }:

{

  home.packages = with pkgs; [
    bugwarrior
  ];

  imports = [
    ./password-store.nix
    ./taskwarrior
  ];

  xdg.configFile."bugwarrior/bugwarriorrc".text = ''
    [general]
    targets = sportradar_jira
    taskrc = ${config.home.homeDirectory}/.taskrc
    inline_links = False
    annotation_links = True
    annotation_comments = True
    legacy_matching = False
    log.level = DEBUG
    log.file = ${config.home.homeDirectory}/log/bugwarrior.log
    annotation_length = 80

    [sportradar_jira]
    service = jira
    jira.base_uri = https://jira.sportradar.ag
    jira.username = e.bailey
    jira.password = @oracle:eval:pass jira.sportradar.ag
    jira.query = ((assignee = currentUser() OR reporter = currentUser()) OR (summary ~ currentUser() OR description ~ currentUser() OR comment ~ currentUser())) AND resolution = Unresolved
    jira.version = 8
    jira.add_tags = work
    jira.description_template = {{jiraid}}: {{jirasummary}}
  '';

}
