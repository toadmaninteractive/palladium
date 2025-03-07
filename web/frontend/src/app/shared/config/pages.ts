import { ConfigModel } from '../interfaces/config';

export class PagesConfig implements ConfigModel {
    public config: any = {};

    constructor() {
        this.config = {
            dashboard: {
                page: {
                    title: 'Dashboard',
                    desc: 'Dashboard description'
                }
            },
            analytics: {
                page: {
                    title: 'Analytics',
                    desc: 'Analytics description'
                }
            },
            events: {
                page: {
                    title: 'Events',
                    desc: 'Events description'
                }
            },
            settings: {
                page: {
                    title: 'Settings',
                    desc: 'Palladium settings'
                }
            },
            404: {
                page: { title: '404 Not Found', desc: '', subheader: false }
            }
        };
    }
}
