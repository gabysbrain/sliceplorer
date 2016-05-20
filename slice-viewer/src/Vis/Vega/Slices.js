// module Vis.Vega.Slices

var React = require('react');
var Component = React.Component;
var PropTypes = React.PropTypes;
var PureRenderMixin = require('react-addons-pure-render-mixin');
var Pux = require('purescript-pux');
var vg = require('vega');

function _spec() {
  return {
    'width': 340,
    'height': 170,
    'data': [{'name': 'lines'}, {'name': 'highlight'}],
    'signals': [{
      'name': 'slicehover', 'init': null,
      'streams': [
        {'type': '@sliceline:mouseover', 'expr': 'datum'},
        {'type': '@sliceline:mouseout', 'expr': 'null'}
      ]
    }],
    'scales': [{
      'name': 'x',
      'type': 'linear',
      'domain': {'data': 'lines', 'field': 'x'},
      'range': 'width'
    }, {
      'name': 'y',
      'type': 'linear',
      'domain': {'data': 'lines', 'field': 'y'},
      'range': 'height'
    }],
    'axes': [{
      'type': 'x',
      'scale': 'x',
      'offset': 5,
      'ticks': 5,
      //'title': 'Distance',
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
      'type': 'group',
      'from': {
        'data': 'lines', 
        'transform': [{'type': 'facet', 'groupby': ['slice_id']}]
      },
      'marks': [{
        'type': 'line',
        'name': 'sliceline',
        'properties': {
          'enter': {
            'x': { 'scale': 'x', 'field': 'x' },
            'y': { 'scale': 'y', 'field': 'y' },
            'interpolate': { 'value': 'basis'},
            'stroke': [{
              'test': "indata('highlight', datum.slice_id, 'slice_id')",
              'value': 'red'
            }, {
              'value': 'black'
            }],
            'strokeWidth': { 'value': 1 },
            'strokeOpacity': [{
              'test': "indata('highlight', datum.slice_id, 'slice_id')",
              'value': 1
            }, { 
              'value': 0.1
            }]
          },
        }
      }]
    }]
  }
};

// from https://gist.github.com/pbeshai/5f0a4d2cd8bb0bc9d96e
var VegaSlices = React.createClass({
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
    var hoverSlice = this.props.hoverSlice;
    var handleHover = this.props.onSliceHover;
    var spec = _spec();
    //spec.axes[0].title = this.props.xAxisName;
    var self = this;

    // parse the vega spec and create the vis
    vg.parse.spec(spec, function(error, chart) {
      var vis = chart({ el: self.refs.chartContainer, renderer: 'canvas' });

      // set the initial data
      if(hoverSlice) {
        vis.data('highlight').insert([hoverSlice]);
      }
      vis.data('lines').insert(data);
      
      // maybe enable hovering
      if(handleHover) {
        vis.onSignal('slicehover', function (_, datum) {
          // This only has a single point in it
          handleHover(datum);
        });
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
    var hoverSlice = this.props.hoverSlice;

    if (vis) {
      // update data in case it changed
      vis.data('highlight').remove(function() {return true;});
      if(hoverSlice) {
        vis.data('highlight').insert([hoverSlice]);
      }
      vis.data('lines').remove(function() {return true;}).insert(data);

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

exports.fromReact = Pux.fromReact(VegaSlices);

