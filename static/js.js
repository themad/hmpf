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
                    config: {}
                };
                if(typeof initData=='object') {
                    $.each(initData, function(k, v) { data.config[k] = v; });
                }
                $this.data('jfhmpf', data);

                $.each(['toggle', 'prev', 'next'], function(i, s) {
                    $this.find('.' + s).click(function() {$this.jfhmpf(s)});
                });

            }
            $.each(['status', 'playlist'], function(i, s) {
                if(data[s].widget.length) $this.jfhmpf(s);
            });

            return this;
        },
        status: function() {
            var data = $(this).data('jfhmpf'),
                widget = data.status.widget;

            $.jpost('/status', {}, function(r) {
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
                list = data.playlist.list,
                tmpl = data.playlist.listItem,
                page = data.playlist.page ? data.playlist.page : 0,
                pages = Math.floor(data.status.mpd_PlaylistLength / data.config.listPageSize);

            widget.find('.prevPage').unbind('click').addClass('disabled');
            widget.find('.nextPage').unbind('click').addClass('disabled');
            if(page>0) {
                widget.find('.prevPage').click(function() {
                    data.playlist.page = page - 1;
                    $this.data('jfhmpf', data);
                    $this.jfhmpf('playlist');
                }).removeClass('disabled');
            }
            if(page<pages) {
                widget.find('.nextPage').click(function() {
                    data.playlist.page = page + 1;
                    $this.data('jfhmpf', data);
                    $this.jfhmpf('playlist');
                }).removeClass('disabled');
            }
            widget.find('.currentPage').html(page + 1);
            widget.find('.lastPage').html(pages + 1);


            $.jget('/list/?count=' + data.config.listPageSize + '&start=' + ((page * data.config.listPageSize) + 1), function(r) {
                widget.find('.listItem').remove();
                $.each(r, function(i, track) {
                    var itm = tmpl.clone();
                    track.Tags.FilePath = track.FilePath;
                    track.Tags.Length = track.Length;
                    $.each(track.Tags, function(key, value) {
                        switch(key) {
                            default:
                                itm.find('.' + key).html(value);
                                break;
                        }
                    });
                    list.append(itm);
                });
            }, 'json');
        },
        toggle: function() {
            var $this=$(this);
            $.jpost('/toggle', {}, function() {

            });
            $this.jfhmpf('status');
        },
        prev: function() {
            var $this=$(this);
            $.jpost('/previous', {}, function() {

            });
            $this.jfhmpf('status');
        },
        next: function() {
            var $this=$(this);
            $.jpost('/next', {}, function() {

            });
            $this.jfhmpf('status');
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
        listPageSize: 10
    });
});