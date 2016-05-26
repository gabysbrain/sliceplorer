// module Vis.Vega.Histogram

var React = require('react');
var Component = React.Component;
var PropTypes = React.PropTypes;
var PureRenderMixin = require('react-addons-pure-render-mixin');
var Pux = require('purescript-pux');
var vg = require('vega');

function _spec() {
  return {
    'width': 100,
    'height': 100,
    'data': [
      {'name': 'bars'}, 
      {'name': 'highlightBar'}, 
      {'name': 'highlightTicks'}
    ],
    'signals': [{
      'name': 'barhover', 'init': null,
      'streams': [
        {'type': '@bar:mouseover', 'expr': 'datum'},
        {'type': '@bar:mouseout', 'expr': 'null'}
      ]
    }],
    'scales': [{
      'name': 'x',
      'type': 'linear',
      'domainMin': {'data': 'bars', 'field': 'bin_start'},
      'domainMax': {'data': 'bars', 'field': 'bin_end'},
      'range': 'width',
      'zero': false
    }, {
      'name': 'y',
      'type': 'linear',
      'domain': {'data': 'bars', 'field': 'count'},
      'range': 'height'
    }],
    'axes': [{
      'type': 'x',
      'scale': 'x',
      'offset': 5,
      //'ticks': 5,
      //'title': 'Distance',
      //'format': 'e',
      'layer': 'back'
    }, {
      'type': 'y',
      'scale': 'y',
      'offset': 5,
      'ticks': 5,
      //'title': 'Value',
      'layer': 'back'
    }],
    'marks': [{
      'type': 'rect',
      'name': 'bar',
      'from': {'data': 'bars'},
      'properties': {
        'enter': {
          'x': { 'scale': 'x', 'field': 'bin_start' },
          'x2': {'scale': 'x', 'field': 'bin_end' },
          'y': { 'scale': 'y', 'field': 'count' },
          'y2': { 'scale': 'y', 'value': 0 },
          'stroke': {'value': 'darkgrey'},
          'strokeWidth': { 'value': 1 },
        },
        'update': {
          'fill': [{
            'test': "indata('highlightBar', datum.bin_start, 'start')",
            'value': 'red'
          }, {
            'value': 'lightgrey'
          }]
        }
      }
    }, {
      'type': 'rule',
      'from': {'data': 'highlightTicks'},
      'properties': {
        'enter': {
          'x': {'scale': 'x', 'field': 'data'},
          //'x': {'value': 50},
          'y': {'value': 0},
          'y2': {'value': 100},
          'stroke': {'value': 'red'},
          'strokeWidth': {'value': 1}
        }
      }
    }]
  }
};

// from https://gist.github.com/pbeshai/5f0a4d2cd8bb0bc9d96e
var VegaHistogram = React.createClass({
  getInitialState: function() {
    // use PureRenderMixin to limit updates when they are not necessary
    this.shouldComponentUpdate = PureRenderMixin.shouldComponentUpdate.bind(this);

    return {vis: null};
  },

  //propTypes: {
    //data: PropTypes.array.isRequired
  //},

  // On initial load, generate the initial vis and attach signal listeners
  componentDidMount: function() {
    var data = this.props.data;
    var highlightBar = this.props.highlightBar;
    var highlightTicks = this.props.highlightTicks;
    var handleHover = this.props.onBarHover;
    var spec = _spec();
    //spec.axes[0].title = this.props.xAxisName;
    var self = this;

    // parse the vega spec and create the vis
    vg.parse.spec(spec, function(error, chart) {
      var vis = chart({ el: self.refs.chartContainer, renderer: 'canvas' });

      // set the initial data
      if(highlightBar) {
        vis.data('highlightBar').insert([highlightBar]);
      }
      if(highlightTicks) {
        vis.data('highlightTicks').insert(highlightTicks);
      }
      vis.data('bars').insert(data);
      
      // maybe enable hovering
      if(handleHover) {
        vis.onSignal('barhover', function (_, datum) {handleHover(datum);});
      }

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
    var highlightBar = this.props.highlightBar;
    var highlightTicks = this.props.highlightTicks;

    if (vis) {
      // update data in case it changed
      vis.data('highlightBar').remove(function() {return true;});
      vis.data('highlightTicks').remove(function() {return true;});
      if(highlightBar) {
        vis.data('highlightBar').insert([highlightBar]);
      }
      if(highlightTicks) {
        vis.data('highlightTicks').insert(highlightTicks);
      }
      //vis.data('bars').remove(function() {return true;}).insert(data);

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

exports.fromReact = Pux.fromReact(VegaHistogram);

