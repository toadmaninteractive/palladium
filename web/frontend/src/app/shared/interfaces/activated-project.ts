import * as WebProtocol from '../../protocol/web-protocol';

export interface ActivatedProject {
    project: WebProtocol.ProjectConfig;
    database: string;
}
