$(function() {
    var photos, bodyWidth, rows, buildRows, resizeRows, border;

    border = 1;
    photos = $('.img');
    bodyWidth = $('.images').innerWidth();

    buildRows = function(photos) {
        var total = 0;
        var currentRow = [];
        var width = 0;
        var rows = [];
        $.each(photos, function() {
            width = $(this).width();
            total += width;

            if (total * 1.1 > bodyWidth) {
                rows.push(currentRow)
                currentRow = [];
                total = width;
            }

            currentRow.push($(this))
        });

        rows.push(currentRow);
        return rows;
    };

    resizeRows = function(rows) {
        var row,
            total,
            i, j,
            len, jlen,
            pic,
            widthRatio,
            oldWidth,
            newWidth,
            oldHeight,
            widthToHeight;

        for (i = 0, len = rows.length; i < len; i++) {
            row = rows[i];
            total = 0;

            for (j = 0, jlen = row.length; j < jlen; j++) {
                total += row[j].width() + border * 2;
            }

            widthRatio = bodyWidth / total;

            for (j = 0, jlen = row.length; j < jlen; j++) {
                pic = row[j];
                oldWidth = pic.width();
                oldHeight = pic.height();
                widthToHeight = oldWidth / oldHeight;
                newWidth = oldWidth * widthRatio;

                pic.css({
                    width: newWidth,
                    height: newWidth / widthToHeight
                });
            }
        }
    };

  resizeRows(buildRows(photos));
});
