{ config, pkgs, ... }:

{

  home.packages = with pkgs; [
    bugwarrior
  ];

  imports = [
    ./password-store.nix
    ./taskwarrior
  ];

  programs.taskwarrior = {
    config = {
      context.other = "jiraurl.none or -work";
      context.work = "jiraurl.any or +work";
      uda = {
        jiracreatedts = {
          label = "Created At";
          type = "date";
        };
        jiradescription = {
          label = "Jira Description";
          type = "string";
        };
        jiraestimate = {
          label = "Estimate";
          type = "numeric";
        };
        jirafixversion = {
          label = "Fix Version";
          type = "string";
        };
        jiraid = {
          label = "Jira Issue ID";
          type = "string";
        };
        jiraissuetype = {
          label = "Issue Type";
          type = "string";
        };
        jirastatus = {
          label = "Jira Status";
          type = "string";
        };
        jirasummary = {
          label = "Jira Summary";
          type = "string";
        };
        jiraurl = {
          label = "Jira URL";
          type = "string";
        };
      };
    };
  };

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
