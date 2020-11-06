{ config, pkgs, ... }:

{

  home.packages = with pkgs; [
    bugwarrior
  ];

  imports = [
    ./password-store.nix
    ./taskwarrior
    ./password-store.nix
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
    jira.query = ((assignee = e.bailey OR reporter = e.bailey) OR (summary ~ e.baley OR description ~ e.bailey OR comment ~ e.bailey)) AND resolution = Unresolved
    jira.version = 8
    jira.add_tags = work
    jira.description_template = {{jiraid}}: {{jirasummary}}
  '';

}
