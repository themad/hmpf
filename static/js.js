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
                        listItem: $this.find('.listBox .list .listItem').detach(),
                    },
                    files: {
                        widget: $this.find('.filesBox'),
                        list: $this.find('.filesBox .list'),
                        listDirItem: $this.find('.filesBox .list .listDirItem').detach(),
                        listFileItem: $this.find('.filesBox .list .listFileItem').detach(),
                    },
                    config: {}
                };
                if(typeof initData=='object') {
                    $.each(initData, function(k, v) { data.config[k] = v; });
                }

                $.each(['toggle', 'prev', 'next'], function(i, s) {
                    $this.find('.' + s).click(function() {$this.jfhmpf(s)});
                });

                if(data['status'].widget.length) {
                    data.status.interval = setInterval(function() { $(this).jfhmpf('status') }, data.config.statusInterval);
                }

                $this.data('jfhmpf', data);
            }
            $.each(['status', 'playlist', 'files'], function(i, s) {
                if(data[s].widget.length) $this.jfhmpf(s);
            });

            return this;
        },
        status: function() {
            var data = $(this).data('jfhmpf'),
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
                            widget.find('.Song').trackInfo(value);
                            break;
                        case 'Consume':case 'Random':case 'Repeat': case 'Single': case 'Consume':
                            widget.find('.' + key).addClass(value ? 'true' : 'false')
                            break;
                        default:
                            widget.find('.' + key).html(value);
                            break;
                    }
                });
                $(this).data('jfhmpf', data);
            });
        },
        playlist: function() {
            var $this = $(this);
                data = $this.data('jfhmpf'),
                widget = data.playlist.widget,
                tmpl = data.playlist.listItem;

            widget.paginatedList(data.config.listPageSize, function(page, pageSize) {
                var list = $(this);
                $.jget('/list' + '?count=' + pageSize + '&start=' + ((page * pageSize) + 1), function(r) {
                    list.find('.listItem').remove();
                    $.each(r, function(i, track) {
                        var itm = tmpl.clone();
                        if(track.Id==data.status.mpd_SongID) {
                            itm.addClass('current')
                        }
                        list.append(itm.trackInfo(track).click(function() { 
                            $this.jfhmpf('play', track.Id + '');
                            $this.jfhmpf('status');
                            $this.jfhmpf('playlist');
                        }));
                    });
                }, 'json');

                return { total:data.status.mpd_PlaylistLength }; // TODO neue hmpf version abwarten
            });
        },
        files: function(path) {
            var $this = $(this);
                data = $this.data('jfhmpf'),
                widget = data.files.widget,
                tmplDir = data.files.listDirItem,
                tmplFile = data.files.listFileItem;

            widget.paginatedList(data.config.listPageSize, function(page, pageSize) {
                var list = $(this), total;
                data.files.path = path ? path : '';
                $.jget('/files' + data.files.path + '?count=' + pageSize + '&start=' + ((page * pageSize) + 1), function(r) {
                    total = r.length*3; // TODO auf neue hmpf Version warten
                    $this.data('jfhmpf', data),
                    list.find('.listDirItem').add('.listFileItem').remove();
                    if(data.files.path.length) {
                        r.unshift({ Directory: '..'})
                    }
                    $.each(r, function(i, v) {
                        if(v.Directory) {
                            var itm = tmplDir.clone();
                            itm.click(function() { $this.jfhmpf('files',  v.Directory=='..' ? path.substr(0, path.lastIndexOf('/')): '/' + v.Directory); });
                            // TODO: Add Dir
                        } else if(v.Song) {
                            var itm = tmplFile.clone();
                            v = v.Song;
                            // TODO: Add file
                        }
                        list.append(itm.trackInfo(v));
                    });
                }, 'json');
                return { total:total };
            });
        },
        play: function(song) {
            var $this=$(this);
            $.jpost('/play' + (song ? '/' + song : ''), {}, function() {

            });
        },
        toggle: function() {
            var $this=$(this);
            $.jpost('/toggle', {}, function() {
            });
        },
        prev: function() {
            var $this=$(this);
            $.jpost('/previous', {}, function() {

            });
        },
        next: function() {
            var $this=$(this);
            $.jpost('/next', {}, function() {

            });
        },
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
        paginatedList: function(pageSize, listFunc) {
            var widget = $(this),
                data = widget.data('paginatedList');

            if(typeof data=='undefined') {
                data = { page:0 };
                widget.data('paginatedList', data);
            }
            var page = data.page,
                list = listFunc.apply(widget.find('.list'), [page, pageSize]),
                pages = Math.floor(list.total / pageSize);

            widget.find('.prevPage').unbind('click').addClass('disabled');
            widget.find('.nextPage').unbind('click').addClass('disabled');

            if(page>0) {
                widget.find('.prevPage').click(function() {
                    data.page = page - 1;
                    widget.data('paginatedList', data);
                    widget.paginatedList(pageSize, listFunc);
                }).removeClass('disabled');
            }
            if(page<pages) {
                widget.find('.nextPage').click(function() {
                    data.page = page + 1;
                    widget.data('paginatedList', data);
                    widget.paginatedList(pageSize, listFunc);
                }).removeClass('disabled');
            }

            widget.find('.currentPage').html(page + 1);
            widget.find('.lastPage').html(pages + 1);

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
            async: false,
            type: 'POST',
            dataType: 'json',
            success: function(r) {
                func(r);
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