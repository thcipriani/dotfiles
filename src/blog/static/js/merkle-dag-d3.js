if (!String.prototype.endsWith) {
  String.prototype.endsWith = function(searchString, position) {
      var subjectString = this.toString();
      if (typeof position !== 'number' || !isFinite(position) || Math.floor(position) !== position || position > subjectString.length) {
        position = subjectString.length;
      }
      position -= searchString.length;
      var lastIndex = subjectString.indexOf(searchString, position);
      return lastIndex !== -1 && lastIndex === position;
  };
}

(function() {
    // http://bl.ocks.org/mbostock/1138500
    var makeGraph  = function(target, graphData) {
        var target = d3.select(target),
            bounds = target.node().getBoundingClientRect(),
            fill   = d3.scale.category20(),
            radius = 25;

        var svg = target.append('svg')
            .attr('width', bounds.width)
            .attr('height', bounds.height);

        // Arrow marker for end-of-line arrow
        svg.append('defs').append('marker')
            .attr('id', 'arrowhead')
            .attr('refX', 17.5)
            .attr('refY', 2)
            .attr('markerWidth', 8)
            .attr('markerHeight', 4)
            .attr('orient', 'auto')
            .attr('fill', '#ccc')
            .append('path')
            .attr('d', 'M 0,0 V 4 L6,2 Z');

        var link = svg.selectAll('line')
            .data(graphData.links)
            .enter()
                .append('line')
                    .attr('class', 'link')
                    .attr('marker-end', 'url(#arrowhead)');

        // Create a group for each node
        var node = svg.selectAll('g')
            .data(graphData.nodes)
            .enter()
                .append('g');

        // Color the node based on node's git-type (otherwise, hot pink!)
        node.append('circle')
            .attr('r', radius)
            .attr('class', 'node')
            .attr('fill', function(d) {
                var blue  = '#1BA1E2',
                    red   = 'tomato',
                    green = '#5BB75B',
                    pink  = '#FE57A1';

                if (d.name.endsWith('.b')) { return red; }
                if (d.name.endsWith('.t')) { return blue; }
                if (d.name.endsWith('.c')) { return green; }
                return pink;
            });

        node.append('text')
            .attr('y', radius * 1.5)
            .attr('text-anchor', 'middle')
            .attr('fill', '#555')
            .text(function(d) {
                if (d.name.length > 10) {
                    return d.name.substring(0, 8) + '...';
                }

                return d.name;
             });

        // If the node has a type: tag it
        node.append('text')
            .attr('text-anchor', 'middle')
            .attr('y', 4)
            .attr('fill', 'white')
            .attr('class', 'bold-text')
            .text(function(d) {
                if (d.name.endsWith('.b')) { return 'BLOB'; }
                if (d.name.endsWith('.t')) { return 'TREE'; }
                if (d.name.endsWith('.c')) { return 'COMMIT'; }
                return '';
             });

        var charge = 700 * graphData.nodes.length;

        var force = d3.layout.force()
            .size([bounds.width, bounds.height])
            .nodes(graphData.nodes)
            .links(graphData.links)
            .linkDistance(150)
            .charge(-(charge))
            .gravity(1)
            .on('tick', tick);

        // No fancy animation, tick amount varies based on number of nodes
        force.start();
        for (var i = 0; i < graphData.nodes.length * 100; ++i) force.tick();
        force.stop();

        function tick(e) {
            // Push sources up and targets down to form a weak tree.
            var k = -12 * e.alpha;

            link
                .each(function(d) { d.source.y -= k, d.target.y += k; })
                    .attr('x2', function(d) { return d.source.x; })
                    .attr('y2', function(d) { return d.source.y; })
                    .attr('x1', function(d) { return d.target.x; })
                    .attr('y1', function(d) { return d.target.y; });

            node
                .attr('transform', function(d) {
                    return 'translate(' + d.x + ',' + d.y + ')';
                });
        }
    };

    var forceFormat = function(dag) {
        var orderedNodes = [],
            nodes = [],
            links = [],
            usesPack = false;

        // Basically a dumb Object.keys
        for (node in dag) {
            if ( !dag.hasOwnProperty( node ) ) continue;
            orderedNodes.push(node);
        }

        orderedNodes.forEach(function(node) {
            var sources = dag[node];

            if (!sources) return;

            sources.forEach(function(source) {
                var source = orderedNodes.indexOf(source);

                // If the source isn't in the DAG, it's in a packfile
                if (source < 0) {
                    if (usesPack) return;
                    source = orderedNodes.length;
                    usesPack = true;
                }

                links.push({
                    'source': source,
                    'target': orderedNodes.indexOf(node)
                });
            });
            nodes.push({'name': node});
        });

        // Add pack file to end of list
        if (usesPack) nodes.push({'name': 'PACK'});

        return { 'nodes': nodes, 'links': links };
    };

    var dag = {
        'Node A': ['Node B'],
        'Node B': []
    };

    var gitDag = {
        // blob
        '1b9f426a8407ffee551ad2993c5d7d3780296353.b': [],
        // tree is a hash that includes the hash from blob
        '098e6de29daf4e55f83406b49f5768df9bc7d624.t': ['1b9f426a8407ffee551ad2993c5d7d3780296353.b'],
        // commit is a hash that includes the hash from tree
        '1a06ce381ac14f7a5baa1670691c2ff8a73aa6da.c': ['098e6de29daf4e55f83406b49f5768df9bc7d624.t'],
    };

    var forceInput = forceFormat(dag);

    makeGraph('.merkle-1', forceInput);
    makeGraph('.merkle-2', forceFormat(gitDag));

    gitDag = { '098e6de29daf4e55f83406b49f5768df9bc7d624.t': [ '1b9f426a8407ffee551ad2993c5d7d3780296353.b' ],
        '1a06ce381ac14f7a5baa1670691c2ff8a73aa6da.c': [ '098e6de29daf4e55f83406b49f5768df9bc7d624.t' ],
        '1b9f426a8407ffee551ad2993c5d7d3780296353.b': [],
        'da94af3a21ac7e0c875bbbe6162aa1d26d699c73.c': [ '098e6de29daf4e55f83406b49f5768df9bc7d624.t' ] }

    makeGraph('.merkle-3', forceFormat(gitDag));

    gitDag = { '098e6de29daf4e55f83406b49f5768df9bc7d624.t': [ '1b9f426a8407ffee551ad2993c5d7d3780296353.b' ],
        '19d9cc8584ac2c7dcf57d2680375e80f099dc481.b': [],
        '1a06ce381ac14f7a5baa1670691c2ff8a73aa6da.c': [ '098e6de29daf4e55f83406b49f5768df9bc7d624.t' ],
        'da94af3a21ac7e0c875bbbe6162aa1d26d699c73.c': [ '098e6de29daf4e55f83406b49f5768df9bc7d624.t' ],
        '1b9f426a8407ffee551ad2993c5d7d3780296353.b': [] }

    makeGraph('.merkle-4', forceFormat(gitDag));

    gitDag = {'098e6de29daf4e55f83406b49f5768df9bc7d624.t': [ '1b9f426a8407ffee551ad2993c5d7d3780296353.b' ],
        '19d9cc8584ac2c7dcf57d2680375e80f099dc481.b': [],
        '1a06ce381ac14f7a5baa1670691c2ff8a73aa6da.c': [ '098e6de29daf4e55f83406b49f5768df9bc7d624.t' ],
        '1b9f426a8407ffee551ad2993c5d7d3780296353.b': [],
        '4f407b396e6ecbb65de6cf192259c18ecd4d1e9b.c':
         [ '7ce38101e91de29ee0fee3aa9940cc81159e0f8d.t',
             'da94af3a21ac7e0c875bbbe6162aa1d26d699c73.c' ],
        '7ce38101e91de29ee0fee3aa9940cc81159e0f8d.t':
         [ '1b9f426a8407ffee551ad2993c5d7d3780296353.b',
             '19d9cc8584ac2c7dcf57d2680375e80f099dc481.b' ],
        'da94af3a21ac7e0c875bbbe6162aa1d26d699c73.c': [ '098e6de29daf4e55f83406b49f5768df9bc7d624.t' ] }

    makeGraph('.merkle-5', forceFormat(gitDag));

    gitDag = { '03130e2de81ebcb11b1e232afe5574fbdd3815a4.t':
         [ '66f4c89da071fc1142bd7fcb27e6509a61c274db.b',
           '95200bfeb1a93e3474b4e90b0ba219130c937962.b',
           '08c5438a68f03db797b83e9e11239e11eafc1a1d.b' ],
        '08c5438a68f03db797b83e9e11239e11eafc1a1d.t':
         [ 'ebeb2ba122b6edabf22f4b7b417af3c6fbc9dd04.b',
           '5981e545bad1a22d86827db34580284d35b0cebb.b' ],
        '143d8ae464edd3dee6f717e0cd7036e00321f888.t':
         [ 'ebeb2ba122b6edabf22f4b7b417af3c6fbc9dd04.b',
           '3273e4f2e01a2e16aea0f4a401362a6587ffa2cd.b' ],
        '155ddfeb60a756cb8936c126ca0147b2e5f73054.c':
         [ '6eb661e38e2818ead906c90027c829bbc6bda1d1.t',
           'c5b0ef9d9bd0d5607cfa767414e5127da82e129d.c' ],
        '2a404e62e6612006ea46ed2774704aec979ea184.t':
         [ '733c072369ca77331f392c40da7404c85c36542c.b',
           '66f4c89da071fc1142bd7fcb27e6509a61c274db.b',
           '95200bfeb1a93e3474b4e90b0ba219130c937962.b',
           '08c5438a68f03db797b83e9e11239e11eafc1a1d.b' ],
        '305599d950101093307783b67458937de0f8980d.t':
         [ '733c072369ca77331f392c40da7404c85c36542c.b',
           '66f4c89da071fc1142bd7fcb27e6509a61c274db.b',
           '95200bfeb1a93e3474b4e90b0ba219130c937962.b',
           '6e5fecc78cebf2585790b226b287a1cee7d14449.b' ],
        '516169f39dfffeef9067764536153a885d313810.c':
         [ '5f0e154d44bdba3a9af41d4e84c97e9352956f15.t',
           '9655225e5b32bed21dcfa29aa36c60258c83cf15.c' ],
        '3273e4f2e01a2e16aea0f4a401362a6587ffa2cd.b': [],
        '5f0e154d44bdba3a9af41d4e84c97e9352956f15.t':
         [ '733c072369ca77331f392c40da7404c85c36542c.b',
           '66f4c89da071fc1142bd7fcb27e6509a61c274db.b',
           '95200bfeb1a93e3474b4e90b0ba219130c937962.b',
           '143d8ae464edd3dee6f717e0cd7036e00321f888.b' ],
        '5981e545bad1a22d86827db34580284d35b0cebb.b': [],
        '62c1c4bc611e4b93dda9474e6916a0fa47227908.c': [ '9935631d78acc1fecf797c83f36f76ae5e326d32.t' ],
        '4b825dc642cb6eb9a060e54bf8d69288fbee4904.t': [],
        '66f4c89da071fc1142bd7fcb27e6509a61c274db.b': [],
        '67b23fed8e7494dc1d8e9ac17b4cecc33242700e.b': [],
        '6e5fecc78cebf2585790b226b287a1cee7d14449.t':
         [ '67b23fed8e7494dc1d8e9ac17b4cecc33242700e.b',
           '3273e4f2e01a2e16aea0f4a401362a6587ffa2cd.b' ],
        '6eb661e38e2818ead906c90027c829bbc6bda1d1.t':
         [ '733c072369ca77331f392c40da7404c85c36542c.b',
           '66f4c89da071fc1142bd7fcb27e6509a61c274db.b',
           '95200bfeb1a93e3474b4e90b0ba219130c937962.b',
           '8cd44698ca06a3ee1ba2494a678fafd727167107.b' ],
        '7e4064c25cccfe4c6747daeeec9ab33b718a1226.c': [ '03130e2de81ebcb11b1e232afe5574fbdd3815a4.t' ],
        '8cd44698ca06a3ee1ba2494a678fafd727167107.t':
         [ 'dbcd2290cb376f5b0095d2f67b42e126665f5dcb.b',
           '5981e545bad1a22d86827db34580284d35b0cebb.b' ],
        '95200bfeb1a93e3474b4e90b0ba219130c937962.b': [],
        '9655225e5b32bed21dcfa29aa36c60258c83cf15.c':
         [ '2a404e62e6612006ea46ed2774704aec979ea184.t',
           '62c1c4bc611e4b93dda9474e6916a0fa47227908.c' ],
        '9935631d78acc1fecf797c83f36f76ae5e326d32.t': [ '733c072369ca77331f392c40da7404c85c36542c.b' ],
        'c5b0ef9d9bd0d5607cfa767414e5127da82e129d.c':
         [ '305599d950101093307783b67458937de0f8980d.t',
           '516169f39dfffeef9067764536153a885d313810.c' ],
        'dbcd2290cb376f5b0095d2f67b42e126665f5dcb.b': [],
        'ebeb2ba122b6edabf22f4b7b417af3c6fbc9dd04.b': [],
        '733c072369ca77331f392c40da7404c85c36542c.b': [] }

    makeGraph('.merkle-6', forceFormat(gitDag));
})();
