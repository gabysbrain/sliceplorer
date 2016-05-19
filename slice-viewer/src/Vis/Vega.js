// module Vis.Vega

var React = require('react');
var Component = React.Component;
var PropTypes = React.PropTypes;
var PureRenderMixin = require('react-addons-pure-render-mixin');
var Pux = require('purescript-pux');
var vg = require('vega');

var multiLineSpec = {
  'width': 170,
  'height': 170,
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
    },
    {
      'name': 'color',
      'type': 'ordinal',
      'domain': {'data': 'points', 'field': 'd'},
      'range': 'category10'
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
  'legends': [
    {
      'fill': 'color', 
      'title': 'dim',
      'orient': 'left'
    }
  ],
  'marks': [
    {
      'type': 'group',
      'from': {
        'data': 'points', 
        'transform': [{'type': 'facet', 'groupby': ['d']}]
      },
      'marks': [
        {
          'type': 'line',
          'properties': {
            'enter': {
              'x': { 'scale': 'x', 'field': 'x' },
              'y': { 'scale': 'y', 'field': 'y' },
              'stroke': {'scale': 'color', 'field': 'd'},
              'strokeWidth': { 'value': 2 },
              'interpolate': { 'value': 'basis'}
            }
          }
        }
      ]
    }
  ]
};

var lineSpecBase = {
  'width': 170,
  'height': 170,
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
      'title': 'x',
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

// from https://gist.github.com/pbeshai/5f0a4d2cd8bb0bc9d96e
var VegaChart = React.createClass({
  getInitialState: function() {
    // use PureRenderMixin to limit updates when they are not necessary
    this.shouldComponentUpdate = PureRenderMixin.shouldComponentUpdate.bind(this);

    return {vis: null};
  },

  propTypes: {
    spec: PropTypes.object.isRequired,
    data: PropTypes.array.isRequired
  },

  // On initial load, generate the initial vis and attach signal listeners
  componentDidMount: function() {
    var data = this.props.data;
    var spec = this.props.spec;
    //spec.axes[0].title = this.props.xAxisName;
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
    var props = Object.assign({}, this.props, {ref: 'chartContainer'});
    return (
      React.createElement("div", props)
    );
  }
});

exports.lineSpec = function(xAxisName) {
  var s = Object.assign({}, lineSpecBase);
  s.axes[0].title = xAxisName;
  return s;
};
exports.multiLineSpec = multiLineSpec;
exports.fromReact = Pux.fromReact(VegaChart);

