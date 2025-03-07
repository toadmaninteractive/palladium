export class Constants {
    public static readonly companyTitle = 'Your Company';
    public static readonly companyWebsiteUrl = 'https://www.yourcompany.com';
    public static readonly companyEmailDomain = 'yourcompany.com';
    public static readonly yearFrom = 2016;
    public static readonly yearTo?: number = new Date().getFullYear() > Constants.yearFrom ? new Date().getFullYear() : null;
    public static readonly slackTeamId = 'XXXXXXXXX';
    public static readonly slackDeveloperTeamId = 'YYYYYYYYY';
    public static readonly slackDeveloperTeamUrl = `slack://channel?team=${Constants.slackTeamId}&id=${Constants.slackDeveloperTeamId}`;
    public static readonly slackDeveloperLeadId = 'ZZZZZZZZZ';
    public static readonly slackDeveloperLeadDmUrl = `slack://user?team=${Constants.slackTeamId}&id=${Constants.slackDeveloperLeadId}`;
    public static readonly slackSysadminId = '123456789';
    public static readonly slackSysadminDmUrl = `slack://user?team=${Constants.slackTeamId}&id=${Constants.slackSysadminId}`;
    public static readonly jiraUrlPrefix = 'https://jira.yourcompany.com/browse/';
    public static readonly storedRouteKey = '$storedRoute';
    public static readonly lastProjectId = '$lastProjectId';
    public static readonly lastDatabase = '$lastDatabase';
    public static readonly loginUrl = '/login';
    public static readonly defaultUrl = '/';
}
