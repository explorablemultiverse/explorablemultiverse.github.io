$(document).ready(function() { console.clear() });

// Defining some global variables which are going to be useful for storing the data
var samples, j, priors, log_ll, data,
	single_prior_densities = {}, 
	single_posterior_densities = {},

	// currently computing manually based on default values to tangle- can be initialized on setup
	prior_weight = [0, 0.25, 0.25, 0, 0, 0.25, 0.25, 0], 
	prior_weight_grid = [],

	//global variable for weights
	weight_coords = {x: 40, y: 50},

	//variables for prior weight setting widget
	input = {h: 200, w: 225}, // dimensions of the input-pad for plotting the posterior
	margin_input = {top: 30, right: 30, bottom: 30, left: 30}, // margins for the input-pad for posterior weights
	prior_loc = [ [margin_input.left, (margin_input.top - 10)], 
					[(input.w - margin_input.right), (margin_input.top - 10)], 
					[margin_input.left, input.h - 10], 
					[(input.w - margin_input.right), input.h - 10] ];

// temporary variable created for debugging median / mode bug
var temp_mixture_dens;

//global variables for estiamates specific to this paper
var ROPE = [-3.03, 3.03],
	dens_in_ROPE = 68.53,
	median = [2.54],
	cuddy_effect_size = 0.6,
	meta_analysis_effect_size = 0.23,
	sd_pooled = 15.16;

var bounding_width = d3.select('div#graph').node().getBoundingClientRect().width;

setSVG(bounding_width);

$(document).on('layoutChanged', function () {
	d3.select('rect')
		.on('mousedown', function(d){
			if (d3.mouse(this)[0] >= margin_input.left && d3.mouse(this)[0] <= (input.w - margin_input.right) && 
				(d3.mouse(this)[1] >= margin_input.top) && (d3.mouse(this)[1] <= (input.w - margin_input.right))) {

				set_widget_weight(d3.mouse(this)[0], d3.mouse(this)[1]);

			}
		})

	d3.select('circle')
		.call(d3.drag()
		.on("drag", function(d){
			if (d3.event.x > margin_input.left && d3.event.x < (input.w - margin_input.right)) {
				sketchpad_x = d3.event.x;
			} else if (d3.event.x <= margin_input.left) {
				sketchpad_x = margin_input.left;
			} else {
				sketchpad_x = (input.w - margin_input.right);
			}

			if (d3.event.y > margin_input.top && d3.event.y < (input.h - margin_input.bottom)) {
				sketchpad_y = d3.event.y;
			} else if (d3.event.y <= margin_input.top) {
				sketchpad_y = margin_input.top;
			} else {
				sketchpad_y = (input.h - margin_input.bottom);
			}

			set_widget_weight(sketchpad_x, sketchpad_y);
			//widget_update_weights(sketchpad_x, sketchpad_y);
		}));
});

function setSVG(width){
	d3.selectAll('svg').remove();

	// Defining variables for plotting the graphs
	var canvas = {w: width, h: width * 10/16}, // dimensions of the canvas for plotting the posterior
		margin = {top: 60, right: 30, bottom: 40, left: 60}; // margins for the canvas for plotting the posterior

	svg = d3.select('div#graph')
			.append('svg')
			.attr('width', canvas.w)
			.attr('height', canvas.h);

	group = svg.append('g');

	g = group.append('g')
			.attr('class', 'graph');

	sketchpad = //d3.select('div#sketchpad')
			//.append('svg')
			group.append('g')
				.attr('class', 'input')
				.attr('width', input.w)
				.attr('height', input.h);

	x = d3.scaleLinear()
			.domain([-20, 20])
			.range([margin.left, canvas.w - margin.right]),

	y = d3.scaleLinear()
			.domain([0, 0.4])
			.range([canvas.h - margin.bottom, margin.top]),

	input_x = d3.scaleLinear()
			.domain([0, 100])
			.range([margin_input.left, input.w - margin_input.right]),

	input_y = d3.scaleLinear()
			.domain([100, 0])
			.range([input.h - margin_input.bottom, margin_input.top]),

	color = d3.scaleOrdinal()
				.range(["#54BA94", "#A0C4E2", "#FF8552",  "#297373", "#59ADDE"]);

	g.append("g")
		.attr("class", "axis axis--x")
		.attr("transform", "translate(0," + (canvas.h - margin.bottom) + ")")
		.call(d3.axisBottom(x))
		.append("text")
		.attr("x", canvas.w - margin.right)
		.attr("y", -6)
		.attr("fill", "#000")
		.attr("text-anchor", "end");
	//	.text("estimates");

	// g.append("g")
	// 	.attr("class", "axis axis--y")
	// 	.attr("transform", "translate(" + margin.left + ",0)")
	// 	.call(d3.axisLeft(y).ticks(null));

	// g.append('text')
	// 	.attr('class', 'legend prior-legend')
	// 	.text('Prior densities')
	// 	.attr('x', (x.range()[0] * 1.5))
	// 	.attr('y', (y.range()[1] * 1.5))
	// 	.attr('font-size', '16px');

	// g.append('path')
	// 	.attr('transform', 
	// 		'translate(' + (x.range()[0] * 1.5 + d3.select('.prior-legend')._groups[0][0].getComputedTextLength() + 8) + ',' 
	// 		+ (y.range()[1] * 1.5 - 4) + ')')
	// 	.attr('d', 'M0 0 L50 0')
	// 	.attr('stroke', '#979797')
	// 	.style('stroke-dasharray', 2)
	// 	.attr('stroke-width', 1);

	g.append('text')
		.attr('class', 'legend prior-mixture-legend')
		.text('Prior density')
		.attr('x', (x.range()[1] * 0.75))
		.attr('y', (y.range()[1] * 0.4))
		.attr('font-size', '16px');

	g.append('path')
		.attr('transform', 
			'translate(' + (x.range()[1] * 0.75 + d3.select('.prior-mixture-legend')._groups[0][0].getComputedTextLength() + 8) + ',' 
			+ (y.range()[1] * 0.4 - 4) + ')')
		.attr('d', 'M0 0 L50 0')
		.attr('stroke', '#dc2127')
		.attr('stroke-width', 2);

	g.append("text")
		.attr("transform", "translate(" + (canvas.w/2) + " ," +  (canvas.h - 8) + ")")
		.style("text-anchor", "middle")
		.text("Mean difference in the number of pumps")
		.attr('font-size', '16px');
}

d3.csv("R/prior_posterior_density_estimates.csv", function(error, d){
	if (error) throw error;

	data = d; // setting it to the globally declared variable so that it can be accessed by functions
	max_y = d3.max(data, function(d) { return d3.max([+d.prior_d]); });
	y.domain( [0, max_y] );

	prior_j = d3.max(data, function(d) { return d.j; });

	log_ll = Object.values(
				d3.nest()
					.key(function(d) { return d.j; })
					.rollup(function(v) { return +v[0].log_C; }) // d3.mean(v, function(d) { return d.log_C; }); })
					.object(data)
			);

	priors = Object.values(d3.nest()
				.key(function(d) { return d.j; })
				.rollup(function(v) { return [+v[0].prior_mu, +v[0].prior_sigma]; })
				.object(data));

	priors_jansen = Object.values(d3.nest()
				.key(function(d) { return d.grid; })
				.rollup(function(v) { return [+v[0].grid, +v[0].prior_d_jansen]; })
				.object(data));

	priors_jansen.shift();

	for (var i = 1; i <= prior_j; i++){
		single_prior_densities[i] = [];
		single_posterior_densities[i] = [];

		// by default, the prior weight would be equal among the mixtures
		// thus, it would be set to 1/J, where J is the number of different priors
		// In this version, J = 4
		if (i % 2){
			prior_weight_grid[Math.floor((i-1)/2)] = (Math.floor((i-1)/2) * 100) / (prior_j/2 - 1);
		}
	}

	data.forEach(function(d, i) {
		single_prior_densities[d.j].push([d.grid, d.prior_d]);
		single_posterior_densities[d.j].push([d.grid, d.post_d]);
	});



	prior_limits = [
						[d3.min(priors.map(x => x[0])), d3.min(priors.map(x => x[1]))],
						[d3.max(priors.map(x => x[0])), d3.min(priors.map(x => x[1]))],
						[d3.min(priors.map(x => x[0])), d3.max(priors.map(x => x[1]))],
						[d3.max(priors.map(x => x[0])), d3.max(priors.map(x => x[1]))]
					];

	for (var i = 0; i < prior_limits.length; i++){
		prior_text = sketchpad.append('text')
			.style('font-weight', 'bold')
			.style('font-size', '12px')
			.attr('text-anchor', 'middle');

		prior_text.append('tspan')
			.attr('class', 'prior-'+i)
			.text('N(' + prior_limits[i][0] + ',' + prior_limits[i][1] + ')')
			.style('fill', '#666')
			.attr('x', prior_loc[i][0])
			.attr('y', prior_loc[i][1])
			.attr('dx', 0)
			.attr('dy', 0);
	}

	sketchpad.append('rect')
		.attr('class', 'input-space')
		.attr('x', input_x(0))
		.attr('y', input_y(0))
		.attr('width', (input_x(100) - input_x(0)))
		.attr('height', (input_y(100) - input_y(0)))
		.attr('fill', '#ddd')
		.on('mousedown', function(d){
			if (d3.mouse(this)[0] >= margin_input.left && d3.mouse(this)[0] <= (input.w - margin_input.right) && 
				(d3.mouse(this)[1] >= margin_input.top) && (d3.mouse(this)[1] <= (input.w - margin_input.right))) {

				set_widget_weight(d3.mouse(this)[0], d3.mouse(this)[1]);

			}
		})

	sketchpad.append('circle')
		.attr('class', 'weight-indicator')
		.attr('cx', input_x(40))
		.attr('cy', input_y(50))
		.attr('r', 5)
		.attr('fill', '#333')
		.attr('stroke', '#333')
		.attr('stroke-width', 3)
		.call(d3.drag()
		.on("drag", function(d){
			if (d3.event.x > margin_input.left && d3.event.x < (input.w - margin_input.right)) {
				sketchpad_x = d3.event.x;
			} else if (d3.event.x <= margin_input.left) {
				sketchpad_x = margin_input.left;
			} else {
				sketchpad_x = (input.w - margin_input.right);
			}

			if (d3.event.y > margin_input.top && d3.event.y < (input.h - margin_input.bottom)) {
				sketchpad_y = d3.event.y;
			} else if (d3.event.y <= margin_input.top) {
				sketchpad_y = margin_input.top;
			} else {
				sketchpad_y = (input.h - margin_input.bottom);
			}

			set_widget_weight(sketchpad_x, sketchpad_y);
			//widget_update_weights(sketchpad_x, sketchpad_y);
		}));


	// Draws prior densities in the background
	for (var i = 1; i <= prior_j/2; i++){
		draw_density('graph', single_prior_densities[i], 'prior', 'none', '#dddddd', 1, 2);
	}
	draw_density('graph', priors_jansen, 'prior', 'none', '#EB5B60');

	var post_weights = get_posterior_weights(Object.values(prior_weight), log_ll);

	draw_mixture_density('graph', Object.values(single_prior_densities), 'prior mixture', prior_weight, 'none', '#dc2127');
	draw_mixture_density('graph', Object.values(single_posterior_densities), 'posterior mixture', post_weights);

	effect_size_annotation(cuddy_effect_size);
	effect_size_annotation(meta_analysis_effect_size);

	draw_annotation('graph', "Region of Practical Equivalence = ["+ -1*sd_pooled*0.2 +", "+ sd_pooled*0.2 +"]", 0, 0.03, -150, 240);
	draw_annotation('graph', "Prior by Jansen and Hornbæk", 0, 0.007, -150, 300);
});


function draw_density(group_name, density, dens_class, fillColor = 'none', strokeColor = 'none', strokeWidth, dashArray = 0){
	group = d3.select('g.'+group_name);

	group.append("path")
		.datum(density)
		.attr('class', dens_class)
		.attr("fill", "none")
		.attr("stroke", "#000")
		.attr("stroke-width", 1.5)
		.attr("stroke-linejoin", "round")
		.attr("d",  d3.line()
		.curve(d3.curveBasis)
		.x(function(d) { return x(d[0]); })
		.y(function(d) { return y(d[1]); }))
		.style('fill', fillColor)
		.style('fill-opacity', 0.4)
		.style('stroke', strokeColor)
		.style('stroke-dasharray', dashArray)
		.style('stroke-width', strokeWidth);
}

function get_log_sum_exp(ll_sum){
	return Math.log(ll_sum.map( x => Math.exp(x - Math.max(...ll_sum))).reduce((a, b) => a + b)) + Math.max(...ll_sum);
}

function get_posterior_weights(prior_weights, log_ll) {
	prior_weight_log = Object.values(prior_weights).map( x => Math.log(x) );
	ll_sum = math.add(prior_weight_log, Object.values(log_ll));
	log_sum_exp = get_log_sum_exp(ll_sum);

	return ll_sum.map( x => Math.exp(x - log_sum_exp));
}

function draw_mixture_density(group_name, densities, dens_class, weights, fillColor = '#B9E3FF', strokeColor = 'none', dashArray = 0){	
	var single_weighted_densities = [];
	var x_ticks = densities[0].map(x => x[0]);

	for (var i = 0; i < densities.length; i++){
		single_weighted_densities[i] = densities[i].map( x => math.multiply(x[1], weights[i]));
	}

	const add = (a, b) => (a + b);

	var mixture_density = _.zip( x_ticks, _.unzip(single_weighted_densities).map(x => x.reduce(add)) );

	draw_density(group_name, mixture_density, dens_class, fillColor, strokeColor, 2, dashArray);

	if (dens_class == 'posterior mixture'){
		temp_mixture_dens = mixture_density;
		dens_in_ROPE = ROPE_density(mixture_density, ROPE);

		var [lower_quantile, upper_quantile] = get_ROPE(mixture_density, ROPE); //get_quantile(mixture_density, 95);
		var [lowerMedian, upperMedian] = get_quantile(mixture_density, 0.1);

		HDI = mixture_density.slice(lower_quantile, upper_quantile);
		HDI.splice(0, 0, [+mixture_density[lower_quantile][0], 0]);
		HDI.push([+mixture_density[upper_quantile][0], 0]);
		draw_density(group_name, HDI, dens_class, '#5D9CCA', 'none', dashArray);

		median = mixture_density[lowerMedian];

		d3.select('g.'+group_name)
			.append('line')
			.attr('class', 'posterior median mixture')
			.attr('x1', x(median[0]))
			.attr('y1', y(median[1]))
			.attr('x2', x(median[0]))
			.attr('y2', y(0))
			.attr('stroke-width', 2)
			.attr('stroke', '#666');
	}
}

function get_quantile(density, interval) {
	var del_x = density.map(x => +x[0]),
		y = density.map(x => +x[1]),
		lower = (1 - (interval/100))/2,
		upper = 1 - lower;

	var sum_lower = 0, sum_upper = 0;
	for (var i = 1; i < density.length; i++){
		if (sum_lower <= lower){
			sum_lower += (del_x[i] - del_x[i-1])*y[i];
		} else {
			break;
		}
	}
	for (var j = 1; j < density.length; j++){
		if (sum_upper < upper){
			sum_upper += (del_x[j] - del_x[j-1])*y[j];
		} else {
			break;
		}
	}

	return [i, j];
}

function get_ROPE(density, interval) {
	var x = density.map(x => +x[0]);
	var lower_idx, upper_idx;

	for (var i = 1; i < density.length; i++){
		if (x[i] <= interval[0]){
			lower_idx = i
		} else if (x[i] <= interval[1]){
			upper_idx = i;
		}
	}

	return [lower_idx, upper_idx]
}

function ROPE_density(density, interval) {
	var x = density.map(x => +x[0]),
		y = density.map(x => +x[1]),
		xmin = interval[0], xmax = interval[1],
		sum = 0;

	for (var i = 1; i < density.length; i++){
		if (x[i] >= xmin && x[i] <= xmax){
			sum += (x[i] - x[i-1])*y[i];
		}
	}
	return sum*100;
}


function set_widget_weight(sketchpad_x, sketchpad_y){
	weight_coords = {x: Math.round(input_x.invert(sketchpad_x)), y: Math.round(input_y.invert(sketchpad_y))};
	widget_weight_indicator(sketchpad_x, sketchpad_y);
	tangle.widget_weights();
}

function widget_weight_indicator(x, y){
	d3.select('.weight-indicator')
		.attr('cx', x)
		.attr('cy', y);
}

function remove_mixture_plots(){
	d3.selectAll('.mixture').remove();
}

function effect_size_annotation(effect_size){
	g.append("line")
		.attr('class', 'mixture effect-size-annotation')
		.attr('x1', x(sd_pooled*effect_size))
		.attr('y1', y(max_y/50))
		.attr('x2', x(sd_pooled*effect_size))
		.attr('y2', y(-1*max_y/50))
		.attr('stroke-width', 2)
		.attr('stroke', 'red');

	const type = d3.annotationCustomType(
		d3.annotationLabel, 
		{"className":"custom",
		"connector":{"type":"elbow"},
		"note":{"align":"middle",
		"orientation":"leftRight"}}
	)

	const annotations = [{
		note: {
			label: "Mean calculated from effect size of d = "+ effect_size +""
		},
		//className: "show-bg",
		x: x(sd_pooled*effect_size),
		y: y(0),
		dy: -120,
		dx: 100,
	}]

	const makeAnnotations = d3.annotation()
		.editMode(false)
		.textWrap(60)
		.notePadding(5)
		.type(type)
		.annotations(annotations);

	d3.select('g').append("g")
		.attr("class", "mixture annotation-group")
		.call(makeAnnotations)
}

function draw_annotation(g, str, annotation_x, annotation_y, annotation_dx, annotation_dy){
	const type = d3.annotationCustomType(
		d3.annotationLabel, 
		{"className":"custom",
		"connector":{"type":"elbow"},
		"note":{"align":"middle",
		"orientation":"leftRight"}}
	)

	const annotations = [{
		note: {
			label: str
		},
		//className: "show-bg",
		x: x(annotation_x),
		y: y(annotation_y),
		dy: -y(annotation_y) + annotation_dy,
		dx: annotation_dx
	}]

	const makeAnnotations = d3.annotation()
		.editMode(false)
		.textWrap(120)
		.notePadding(5)
		.type(type)
		.annotations(annotations);

	d3.select('g').append("g")
		.attr("class", "mixture annotation-group")
		.call(makeAnnotations)
}

// function widget_update_weights(sketchpad_x, sketchpad_y){
// 	coords = {x: input_x.invert(sketchpad_x), y: input_y.invert(sketchpad_y)};
// 	console.log(coords.x, coords.y);

// 	d3.select('.weight-indicator')
// 		.attr('cx', sketchpad_x)
// 		.attr('cy', sketchpad_y);

// 	for (var i=1; i<4; i++){
// 		if (coords.x <= prior_weight_grid[i]){
// 			weight_idx = {1: (i - 1), 2: i, 3: ((i+4) -1), 4: ((i+4))}
			
// 			weight_to_optim = (coords.x - prior_weight_grid[i-1]) * 3;
// 			//weight_to_skeptic = (100 - weight_to_optim);

// 			for (var q = 0; q < prior_weight.length; q++){
// 				prior_weight[q] = 0;
// 			}

// 			prior_weight[weight_idx[1]] = ((100 - weight_to_optim)/ 100 * (100 - coords.y)/100);
// 			prior_weight[weight_idx[2]] = (weight_to_optim/100 * (100 - coords.y)/100);
// 			prior_weight[weight_idx[3]] = ((100 - weight_to_optim)/100 * coords.y/100);
// 			prior_weight[weight_idx[4]] = (weight_to_optim/100 * coords.y/100);
			
// 			break;
// 		}
// 	}

// 	d3.selectAll('.mixture').remove();

// 	prior_weights = Object.values(prior_weight);
// 	var post_weights = get_posterior_weights(prior_weights, log_ll);

// 	draw_mixture_density(g, Object.values(single_prior_densities), 'prior mixture', prior_weights, 'none', 'red');
// 	draw_mixture_density(g, Object.values(single_posterior_densities), 'posterior mixture', post_weights);

// 	effect_size_annotation(cuddy_effect_size);
// 	effect_size_annotation(meta_analysis_effect_size);
// 	ROPE_annotation(meta_analysis_effect_size);

// 	// this.ROPE = dens_in_ROPE;
// 	// this.median_beta = Math.round(median[0] * 100) / 100;
// }

