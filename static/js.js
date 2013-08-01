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
                page = $this.jfhmpf('paginatedList', 'playlist', data.status.mpd_PlaylistLength),
                list = data.playlist.list,
                tmpl = data.playlist.listItem;

            $.jget('/list' + '?count=' + data.config.listPageSize + '&start=' + ((page * data.config.listPageSize) + 1), function(r) {
                widget.find('.listItem').remove();
                $.each(r, function(i, track) {
                    var itm = tmpl.clone();
                    list.append(itm.trackInfo(track).click(function() { $this.jfhmpf('play', track.Id + '')}));
                });
            }, 'json');
        },
        files: function(path) {
            var $this = $(this);
                data = $this.data('jfhmpf'),
                widget = data.files.widget,
                page = $this.jfhmpf('paginatedList', 'files', data.files.lastCount),
                list = data.files.list,
                tmplDir = data.files.listDirItem,
                tmplFile = data.files.listFileItem;

            data.files.path = path ? path : (data.files.path ? data.files.path : '');

            // TODO: Add Back
            $.jget('/files' + data.files.path + '?count=' + data.config.listPageSize + '&start=' + ((page * data.config.listPageSize) + 1), function(r) {
                widget.find('.listDirItem').add('.listFileItem').remove();
                $.each(r, function(i, v) {
                    if(v.Directory) {
                        var itm = tmplDir.clone();
                        itm.click(function() { $this.jfhmpf('files', '/' + v.Directory); });
                        // TODO: Add Dir
                    } else if(v.Song) {
                        var itm = tmplFile.clone();
                        v = v.Song;
                        // TODO: Add file
                    }
                    list.append(itm.trackInfo(v));
                });
            }, 'json');
        },
        paginatedList: function(type, total) {
            var $this = $(this);
                data = $this.data('jfhmpf'),
                widget = data[type].widget,
                page = data[type].page ? data[type].page : 0,
                pages = Math.floor(total / data.config.listPageSize);

            widget.find('.prevPage').unbind('click').addClass('disabled');
            widget.find('.nextPage').unbind('click').addClass('disabled');
            if(page>0) {
                widget.find('.prevPage').click(function() {
                    data[type].page = page - 1;
                    $this.data('jfhmpf', data);
                    $this.jfhmpf(type);
                }).removeClass('disabled');
            }
            if(page<pages) {
                widget.find('.nextPage').click(function() {
                    data[type].page = page + 1;
                    $this.data('jfhmpf', data);
                    $this.jfhmpf(type);
                }).removeClass('disabled');
            }
            widget.find('.currentPage').html(page + 1);
            widget.find('.lastPage').html(pages + 1);

            return page;
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