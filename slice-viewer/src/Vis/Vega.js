// module Vis.Vega

var React = require('react');
var Component = React.Component;
var PropTypes = React.PropTypes;
var PureRenderMixin = require('react-addons-pure-render-mixin');
var Pux = require('purescript-pux');
var vg = require('vega');


var lineSpec = {
  'width': 400,
  'height': 400,
  //'padding': { 'top': 10, 'left': 50, 'bottom': 50, right: 10 },
  'data': [{ 'name': 'points' }],
  'scales': [
    {
      'name': 'x',
      'type': 'linear',
      'domain': { 'data': 'points', 'field': 'x' },
      'range': 'width'
    },
    {
      'name': 'y',
      'type': 'linear',
      'domain': { 'data': 'points', 'field': 'y' },
      'range': 'height',
      'nice': true
    }
  ],
  'axes': [
    {
      'type': 'x',
      'scale': 'x',
      'offset': 5,
      'ticks': 5,
      //'title': 'Distance',
      'layer': 'back'
    },
    {
      'type': 'y',
      'scale': 'y',
      'offset': 5,
      'ticks': 5,
      //'title': 'Value',
      'layer': 'back'
    }
  ],
  'marks': [
    {
      'type': 'line',
      'from': { 'data': 'points' },
      'properties': {
        'enter': {
          'x': { 'scale': 'x', 'field': 'x' },
          'y': { 'scale': 'y', 'field': 'y' },
          'stroke': { 'value': '#5357a1' },
          'strokeWidth': { 'value': 2 },
          'interpolate': { 'value': 'basis'}
        }
      }
    }
  ]
};

exports.vegaParseImpl = vg.parse.spec;
exports.parse2 = function(el, data) {
  var chart = vg.parse.spec(lineSpec);
  chart(el, {"table": data});
};

// from https://gist.github.com/pbeshai/5f0a4d2cd8bb0bc9d96e
var VegaLineChart = React.createClass({
  getInitialState: function() {
    // use PureRenderMixin to limit updates when they are not necessary
    this.shouldComponentUpdate = PureRenderMixin.shouldComponentUpdate.bind(this);

    return {vis: null};
  },

  propTypes: {
    data: PropTypes.array.isRequired
  },

  // On initial load, generate the initial vis and attach signal listeners
  componentDidMount: function() {
    var data = this.props.data;
    var spec = lineSpec;
    var self = this;

    // parse the vega spec and create the vis
    vg.parse.spec(spec, function(error, chart) {
      var vis = chart({ el: self.refs.chartContainer, renderer: 'svg' });

      // set the initial data
      vis.data('points').insert(data);

      // render the vis
      vis.update();

      // store the vis object in state to be used on later updates
      self.setState({vis: vis });
    });
  },

  // updates mean that the data changed
  componentDidUpdate: function() {
    var vis = this.state.vis;
    var data = this.props.data;

    if (vis) {
      // update data in case it changed
      vis.data('points').remove(function() {return true;}).insert(data);

      vis.update();
    }
  },

  // dummy render method that creates the container vega draws inside
  render: function() {
    return (
      React.createElement("div", {ref: "chartContainer"})
    );
  }
});

exports.fromReact = Pux.fromReact(VegaLineChart);

