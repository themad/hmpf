(function($) {
    var jfhmpfMethods = {
        init: function(initData) {
            var $this = $(this),
                data = $this.data('jfhmpf');
            if(!data) {
                data = {
                    root: $this,
                    status: {
                        widget: $this.find('.statusBox .status'),
                    },
                    playlist: {
                        widget: $this.find('.listBox'),
                        list: $this.find('.listBox .list'),
                        listItem: $this.find('.listBox .list .item').detach(),
                    },
                    files: {
                        widget: $this.find('.filesBox'),
                        list: $this.find('.filesBox .list'),
                        crumbs: $this.find('.filesBox .pathCrumbs'),
                        listDirItem: $this.find('.filesBox .list .item.path').detach(),
                        listFileItem: $this.find('.filesBox .list .item.file').detach(),
                        listPlaylistItem: $this.find('.filesBox .list .item.playlist').detach()
                    },
                    config: {}
                };
                if(typeof initData=='object') {
                    $.each(initData, function(k, v) { data.config[k] = v; });
                }

                $.each(['toggle', 'prev', 'next'], function(i, s) {
                    $this.find('.' + s).click(function() { $this.jfhmpf(s) });
                });

                if(data['status'].widget.length) {
                    data.status.interval = setInterval(function() { $this.jfhmpf('status') }, data.config.statusInterval);
                }

                $this.data('jfhmpf', data);
            }
            $.each(['status', 'playlist', 'files'], function(i, s) {
                if(data[s].widget.length) $this.jfhmpf(s);
            });

            return this;
        },
        status: function() {
            var $this = $(this),
                data = $this.data('jfhmpf'),
                widget = data.status.widget;

            $.jget('/status', function(r) {
                $.each(r, function(key, value) {
                    data.status['mpd_' + key] = value;
                    switch(key) {
                        case 'Time':
                            var prgrs = widget.find('.TimeProgress'),
                                w = prgrs.width(),
                                done = value[0]/value[1];
                            prgrs.find('.done').css({width:(done*w) + 'px'});
                            prgrs.find('.notdone').css({width:((1-done)*w) + 'px'});
                            break;
                        case 'State':
                            var stt = widget.find('.State');
                            $.each(value, function(state) { stt.html(state); });
                            break;
                        case 'Song':
                            if(value) widget.find('.Song').trackInfo(value);
                            break;
                        case 'Consume':case 'Random':case 'Repeat': case 'Single': case 'Consume':
                            widget.find('.' + key).removeClass(!value ? 'true' : 'false').addClass(value ? 'true' : 'false').unbind('click').click(function() {
                                $this.jfhmpf('statusToggle', key)
                            });
                            break;
                        default:
                            widget.find('.' + key).html(value);
                            break;
                    }
                    if(data.status.lastSongId != data.status.mpd_SongID) {
                        if(data.status.lastSongId) $this.jfhmpf('playlist');
                        data.status.lastSongId = data.status.mpd_SongID;
                    }
                });
                $this.data('jfhmpf', data);
            });
        },
        statusToggle: function(key) {
            var $this = $(this);
                data = $this.data('jfhmpf'),
                p = {};
            p[key] = !data.status['mpd_' + key];
            $.jpost('/status', p);
            $this.jfhmpf('status');
        },
        playlist: function() {
            var $this = $(this);
                data = $this.data('jfhmpf');

            data.playlist.widget.paginatedList('/list', data.config.listPageSize, function(track) {
                var itm = data.playlist.listItem.clone();
                if(track.Id==data.status.mpd_SongID) {
                    itm.addClass('current')
                }
                $(this).append(itm.trackInfo(track));
                itm.find('.playSong').click(function() { $this.jfhmpf('play', track.Index + ''); });
                itm.find('.removeFromPlaylist').click(function() { $this.jfhmpf('delete', track.Index + ''); });
            });
            $this.find('.clearList').unbind('click').click(function() { $this.jfhmpf('clear'); });
        },
        files: function(path) {
            var $this = $(this);
                data = $this.data('jfhmpf'),
                data.files.path = path ? path : '';

            data.files.crumbs.pathCrumbs(data.files.path, function(path) { $this.jfhmpf('files', path); });

            data.files.widget.paginatedList('/files' + data.files.path, data.config.listPageSize, function(file) {
                $this.data('jfhmpf', data);

                if(file.Directory) {
                    var itm = data.files.listDirItem.clone(),
                        add = file.Directory;
                    itm.find('.changeDir').click(function() { $this.jfhmpf('files',  file.Directory=='..' ? path.substr(0, path.lastIndexOf('/')): '/' + file.Directory); });
                    file.File = file.Directory.substring(file.Directory.lastIndexOf('/') + 1);
                } else if(file.Song) {
                    var itm = data.files.listFileItem.clone(),
                        add = file.Song.FilePath;
                    file = file.Song;
                    file.FileName = file.FilePath.substring(file.FilePath.lastIndexOf('/') + 1);
                } else if(file.Playlist) {
                    var itm = data.files.listPlaylistItem.clone(),
                        add = file.Playlist;
                    file.File = file.Playlist.substring(file.Playlist.lastIndexOf('/') + 1);
                }
                itm.find('.addToPlaylist').click(function() {
                    $this.jfhmpf('addToPlaylist', add);
                });
                $(this).append(itm.trackInfo(file));
            });
        },
        play: function(song) {
            var $this=$(this);
            $.jpost('/play' + (song ? '/' + song : ''), {});
            $this.jfhmpf('status');
        },
        delete: function(song) {
            var $this=$(this);
            $.jpost('/delete' + (song ? '/' + song : ''), {});
            $this.jfhmpf('status');
            $this.jfhmpf('playlist');
        },
        clear: function() {
            var $this=$(this);
            $.jpost('/clear', {});
            $this.jfhmpf('status');
            $this.jfhmpf('playlist');
        },
        toggle: function() {
            var $this=$(this);
            $.jpost('/toggle', {});
        },
        prev: function() {
            var $this=$(this);
            $.jpost('/previous', {});
            $this.jfhmpf('status');
        },
        next: function() {
            var $this=$(this);
            $.jpost('/next', {});
            $this.jfhmpf('status');
        },
        addToPlaylist: function(item) {
            var $this=$(this);
            $.jpost('/list/' + encodeURI(item), {});
            $this.jfhmpf('status');
            $this.jfhmpf('playlist');
        }
    };

    $.fn.extend({
        jfhmpf: function(method) {
            if(jfhmpfMethods[method]) {
                return jfhmpfMethods[method].apply(this, Array.prototype.slice.call(arguments, 1));
            } else if(typeof method === 'object' || !method) {
                return jfhmpfMethods.init.apply(this, arguments);
            } else {
                $.error( 'Method ' +  method + ' does not exist on jQuery.jfhmpf' );
            }    
        },
        trackInfo: function(info) {
            var $this = $(this);
            $.each(info, function(key, value) {
                switch(key) {
                    case 'Tags':
                        $this.trackInfo(value);
                        break;
                    default:
                        $this.find('.' + key).html(value);
                        break;
                }
            });
            return $this;
        },
        paginatedList: function(uri, pageSize, listFunc) {
            var widget = $(this),
                data = widget.data('paginatedList'),
                page;

            if(typeof data=='undefined') {
                data = { page:0 };
                widget.data('paginatedList', data);
            }
            page = data.page;

            $.jget(uri + '?count=' + pageSize + '&start=' + (page * pageSize), function(r) {
                var pages = Math.floor(r.total / pageSize);
                widget.find('.list .item').remove();

                $.each(r.result, function(i, item) {
                    listFunc.apply(widget.find('.list'), [item]);
                });

                widget.find('.prevPage').unbind('click').addClass('disabled');
                widget.find('.nextPage').unbind('click').addClass('disabled');

                if(page>0) {
                    widget.find('.prevPage').click(function() {
                        data.page = page - 1;
                        widget.data('paginatedList', data);
                        widget.paginatedList(uri, pageSize, listFunc);
                    }).removeClass('disabled');
                }
                if(page<pages) {
                    widget.find('.nextPage').click(function() {
                        data.page = page + 1;
                        widget.data('paginatedList', data);
                        widget.paginatedList(uri, pageSize, listFunc);
                    }).removeClass('disabled');
                }

                widget.find('.currentPage').html(page + 1);
                widget.find('.lastPage').html(pages + 1);
            });

            return widget;
        },
        pathCrumbs: function (path, func) {
            var widget = $(this),
                crumbs = ('root' + path).split('/'),
                path = '';
            widget.html('/');//find('.crumb').remove();
            $.each(crumbs, function(i, crumb) {
                var itm = $('<span class="crumb"></span>');
                widget.append(itm.html(crumb));
                if(i<(crumbs.length-1)) {
                    path += i==0 ? '' : '/' + crumb;
                    itm.click(func.bind(itm, [path]));
                    widget.append('/');
                } else {
                    itm.addClass('selected');
                }
            });
            return widget;
        }
    });
})(jQuery);

$.extend({
    jget: function(url, func) {
        $.ajax({
            async: false,
            dataType: 'json',
            success: function(r) {
                func(r);
            },
            url: url
        });

        return this;
    },
    jpost: function(url, data, func) {
        $.ajax({
            async: true,
            type: 'POST',
            dataType: 'json',
            success: function(r) {
                if(typeof fucn=='function') func(r);
            },
            url: url,
            data: data
        });

        return this;
    },
});

$(function() {
    $('#jfhmpf').jfhmpf({
        listPageSize: 20,
        statusInterval: 1000
    });
});