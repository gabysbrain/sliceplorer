
// module Vis.D3.SliceChart

var React = require('react');
var PureRenderMixin = require('react-addons-pure-render-mixin');
var Pux = require('purescript-pux');
var d3 = require('d3');

var _chart = function() {
  var x = d3.scaleLinear();
  var y = d3.scaleLinear();

  return {
    width: 340,
    height: 170,
    margin: {left: 35, top: 10, right: 10, bottom: 20},

    x: x,
    y: y,
    color: d3.scaleOrdinal(d3.schemeCategory10),

    xAxis: d3.axisBottom(x),
    yAxis: d3.axisLeft(y),
  };
};

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

function drawSlices(self, chart, data, showClusters) {
  var handleHover = self.props.onSliceHover;
  var line = d3.line()
    .curve(d3.curveBasis)
    .x(function(d) { return self.state.x(d.value0); })
    .y(function(d) { return self.state.y(d.value1); });

  var slices = chart.selectAll('.slice.line').data(data);
  slices.enter()
      .append('g').attr('class', 'slice line')
                  .append('path')
                    .on('mouseover', function() {
                      handleHover(d3.select(this).data());
                    })
                    .on('mouseout', function() {
                      handleHover([]);
                    })
                    .attr('class', 'line')
                    .attr('d', function(d) {return line(d.value0.slice);})
                    .style('fill', 'none')
                    .style('stroke-width', 1)
                    .style('stroke-opacity', 0.1)
                    .style('stroke', function(d) {
                      if(showClusters)
                        return self.state.color(d.value0.clusterId);
                      else
                        return 'black';
                    });
  slices.exit().remove();
}

function drawHighlights(self, chart, data) {
  // Highlights consist of both ticks to show the x values and lines
  var line = d3.line()
    .curve(d3.curveBasis)
    .x(function(d) { return self.state.x(d.value0); })
    .y(function(d) { return self.state.y(d.value1); });

  var slices = chart.selectAll('.slice.highlight').data(data);
  slices.enter()
      .append('g').attr('class', 'slice highlight')
                  .append('path')
                    .attr('class', 'line')
                    .attr('d', function(d) {return line(d.value0.slice);})
                    .style('fill', 'none')
                    .style('stroke-width', 1)
                    .style('stroke-opacity', 1)
                    .style('stroke', 'red');
  slices.exit().remove();

  var ticks = chart.selectAll('.tick.highlight').data(data);
  ticks.enter()
      .append('line').attr('class', 'tick highlight')
                     .attr('x1', function(d) {return self.state.x(d.value0.focusPoint[d.value0.d]);})
                     .attr('x2', function(d) {return self.state.x(d.value0.focusPoint[d.value0.d]);})
                     .attr('y1', self.state.y.range()[0]+5)
                     .attr('y2', self.state.y.range()[0]-5)
                     .style('fill', 'none')
                     .style('stroke-width', 1)
                     .style('stroke-opacity', 1)
                     .style('stroke', 'red');
  ticks.exit().remove();
}

function updateVis(self) {
  var svg = d3.select(self.refs.chartContainer);
  svg.attr('width', self.state.width)
     .attr('height', self.state.height);

  // set up the sizes for things
  var visWidth = self.state.width - self.state.margin.left - self.state.margin.right;
  var visHeight = self.state.height - self.state.margin.top - self.state.margin.bottom;

  var sliceData = self.props["data-data"];
  var highlightData = self.props["data-highlight"];
  var minY = self.props['data-minVal'];
  var maxY = self.props['data-maxVal'];
  var minX = self.props['data-minX'];
  var maxX = self.props['data-maxX'];
  var showClusters = self.props['data-showclusters'];

  // Update axes and scales
  self.state.x.range([0, visWidth])
              .domain([minX, maxX]);
  self.state.y.range([visHeight, 0])
              .domain([0, maxY]);

  var xAxis = svg.select('g.x.axis');
  xAxis.attr('transform', 'translate(0,'+self.state.y.range()[0]+')')
       .call(self.state.xAxis);
  var yAxis = svg.select('g.y.axis');
  yAxis.call(self.state.yAxis);

  var chartContainer = svg.select('g.container');

  drawSlices(self, chartContainer, sliceData, showClusters);
  drawHighlights(self, chartContainer, highlightData);
}

var SliceChart = React.createClass({
  getInitialState: function() {
    this.shouldComponentUpdate = PureRenderMixin.shouldComponentUpdate.bind(this);

    //return {vis: _chart()};
    return _chart();
  },

  componentDidMount: function() {
    createVis(this);
    updateVis(this);
  },

  componentWillUpdate: function(nextProps, nextState) {
    // if we change the clustering then clear the slices
    // doing this as a general update is slooooow
    if(this.props['data-showclusters'] !== nextProps['data-showclusters']) {
      var svg = d3.select(this.refs.chartContainer);
      var chartContainer = svg.select('g.container');

      drawSlices(this, chartContainer, [], nextProps['data-showclusters']);
    }
  },

  componentDidUpdate: function() {
    updateVis(this);
  },

  // dummy render method that creates the container d3 draws inside
  render: function() {
    var props = Object.assign({}, this.props, {ref: 'chartContainer'});
    return (
      React.createElement("svg", props)
    );
  }
  
});

exports.fromReact = Pux.fromReact(SliceChart);

