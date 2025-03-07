import { ConfigModel } from '../interfaces/config';

export class MenuConfig implements ConfigModel {
    public config: any = {};

    constructor() {
        this.config = {
            header: {},
            aside: {
                self: {},
                items: [
                    {
                        title: 'Dashboard',
                        root: true,
                        icon: 'dashboard',
                        page: '/dashboard/:project/:database',
                        dynamic: true,
                    },
                    {
                        title: 'Analytics',
                        root: true,
                        icon: 'insert_chart',
                        page: '/analytics/:project/:database',
                        dynamic: true,
                    },
                    {
                        title: 'Events',
                        root: true,
                        icon: 'pages',
                        page: '/events/:project/:database',
                        dynamic: true,
                    },
                    {
                        title: 'Patch Notes',
                        root: true,
                        icon: 'event_note',
                        page: '/patch-notes/:project/:database',
                        dynamic: true,
                    },
                    { section: 'Personnel', for: 'superadmin' },
                    {
                        title: 'Accounts',
                        root: true,
                        icon: 'people',
                        page: '/personnel/accounts',
                        for: 'superadmin',
                        breadcrumbs: false,
                    },
                    {
                        title: 'Groups',
                        root: true,
                        icon: 'group_work',
                        page: '/personnel/groups',
                        for: 'superadmin',
                        breadcrumbs: false,
                    },
                    { section: 'Admin', for: 'superadmin' },
                    {
                        title: 'Settings',
                        root: true,
                        icon: 'settings',
                        page: '/settings',
                        for: 'superadmin',
                    },
                ]
            }
        };
    }
}
