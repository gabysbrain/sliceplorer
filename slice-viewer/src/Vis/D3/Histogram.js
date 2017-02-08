
// module Vis.D3.Histogram

var React = require('react');
var Component = React.Component;
var PropTypes = React.PropTypes;
var PureRenderMixin = require('react-addons-pure-render-mixin');
var Pux = require('purescript-pux');
var d3 = require('d3');

var _chart = function() {
  var x = d3.scaleLinear();
  var y = d3.scaleLinear();

  return {
    width: 163,
    height: 149,
    margin: {left: 35, top: 10, right: 10, bottom: 40},

    x: x,
    y: y,

    xAxis: d3.axisBottom(x)
             .ticks(5)
             .tickFormat(d3.format('0.2f')),
    yAxis: d3.axisLeft(y)
             .ticks(7)
             .tickFormat(d3.format('.0%'))
  };
}

function drawHisto(self, chart, data) {
  var handleHover = self.props.onBarHover;
  var bars = chart.selectAll('.histbar').data(data),
      x    = self.state.x,
      y    = self.state.y;
  bars.enter()
    .append('rect')
      .attr('class', 'histbar')
      .attr('stroke', 'darkgrey')
      .attr('stroke-width', 1)
      .attr('fill', 'lightgrey')
      .on('mouseover', function() {
        handleHover(d3.select(this).data()[0]);
      })
      .on('mouseout', function() {
        handleHover(null);
      });
  bars
      .attr('x', function(d) {return x(d.bin_start);})
      .attr('width', function(d) {return x(d.bin_end) - x(d.bin_start);})
      .attr('height', function(d) {return y.range()[0] - y(d.percentage);})
      .attr('y', function(d) {return y(d.percentage);})
  bars.exit().remove();
}

function drawHighlights(self, chart, data) {
  var strokes = chart.selectAll('.highlight.stroke').data(data),
      x    = self.state.x,
      y    = self.state.y;
  strokes.enter()
    .append('line')
      .attr('class', 'highlight stroke')
      .attr('x1', function(d) {return x(d);})
      .attr('x2', function(d) {return x(d);})
      .attr('y1', function(d) {return y.range()[0];})
      .attr('y2', function(d) {return y.range()[1];})
      .attr('stroke', 'red')
      .attr('stroke-width', 1);
  strokes.exit().remove();

  var hlBar = self.props['data-highlightBar'];
  hlBar = hlBar === undefined ? [] : [hlBar];
  var bars = chart.selectAll('.highlight.bar').data(hlBar);
  console.log(hlBar);
  bars.enter()
    .append('rect')
      .attr('class', 'highlight bar')
      .attr('x', function(d) {return x(d.start);})
      .attr('width', function(d) {return x(d.end) - x(d.start);})
      .attr('height', function(d) {return y.range()[0] - y(d.percentage);})
      .attr('y', function(d) {return y(d.percentage);})
      .attr('stroke', 'darkgrey')
      .attr('stroke-width', 1)
      .attr('fill', 'red')
  bars.exit().remove();
}

function createVis(self) {
  var svg = d3.select(self.refs.chartContainer);
  svg.attr('class', 'chart');

  var chart = svg.append('g');
  chart.attr('transform', 'translate(' + self.state.margin.left + ',' + self.state.margin.top + ')')
       .attr('class', 'container');

  chart.append('g')
       .attr('class', 'x axis');
  chart.append('g')
       .attr('class', 'y axis');
}

function updateVis(self) {
  var svg = d3.select(self.refs.chartContainer);
  svg.attr('width', self.state.width)
     .attr('height', self.state.height);

  // set up the sizes for things
  var visWidth = self.state.width - self.state.margin.left - self.state.margin.right;
  var visHeight = self.state.height - self.state.margin.top - self.state.margin.bottom;

  var histData = self.props["data-histogram"];
  var highlightData = self.props["data-highlight"];
  var minX = self.props['data-binMin'];
  var maxX = self.props['data-binMax'];
  var maxY = self.props['data-maxCount'];

  // Update axes and scales
  self.state.x.range([0, visWidth])
              .domain([minX, maxX]);
  self.state.y.range([visHeight, 0])
              .domain([0, 1]);

  var xAxis = svg.select('g.x.axis');
  xAxis.attr('transform', 'translate(0,'+self.state.y.range()[0]+')')
       .call(self.state.xAxis)
    .selectAll("text")
      //.attr("y", 0)
      //.attr("x", 9)
      .attr("dx", "-.8em")
      .attr("dy", ".35em")
      .attr("transform", "rotate(-45)")
      .style("text-anchor", "end");
  var yAxis = svg.select('g.y.axis');
  yAxis.call(self.state.yAxis);

  var chartContainer = svg.select('g.container');

  drawHisto(self, chartContainer, histData);
  drawHighlights(self, chartContainer, highlightData);
}

var Histogram = React.createClass({
  getInitialState: function() {
    // use PureRenderMixin to limit updates when they are not necessary
    this.shouldComponentUpdate = PureRenderMixin.shouldComponentUpdate.bind(this);

    return _chart();
  },

  componentDidMount: function() {
    createVis(this);
    updateVis(this);
  },

  /*
  componentWillUpdate: function(nextProps, nextState) {
    // if we change the clustering then clear the slices
    // doing this as a general update is slooooow
    if(this.props['data-showclusters'] !== nextProps['data-showclusters']) {
      var svg = d3.select(this.refs.chartContainer);
      var chartContainer = svg.select('g.container');

      drawSlices(this, chartContainer, [], nextProps['data-showclusters']);
    }
  },
  */

  componentDidUpdate: function() {
    updateVis(this);
  },

  // dummy render method that creates the container vega draws inside
  render: function() {
    var props = Object.assign({}, this.props, {ref: 'chartContainer'});
    return (
      React.createElement("svg", props)
    );
  }
});

exports.fromReact = Pux.fromReact(Histogram);

