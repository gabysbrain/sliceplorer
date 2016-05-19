// module Vis.Vega.Splom

var React = require('react');
var Component = React.Component;
var PropTypes = React.PropTypes;
var PureRenderMixin = require('react-addons-pure-render-mixin');
var Pux = require('purescript-pux');
var vg = require('vega');

function _spec() {
  return {
    'width': 450,
    'height': 450,
    'data': [{'name': 'points'}, {'name': 'fields'}],
    'scales': [{
      'name': 'gx',
      'type': 'ordinal',
      'range': 'width',
      'round': true,
      'domain': {'data': 'fields', 'field': 'data'}
    }, {
      'name': 'gy',
      'type': 'ordinal',
      'range': 'height',
      'round': true,
      'reverse': true,
      'domain': {'data': 'fields', 'field': 'data'}
    }],
    'marks': [{
      'type': 'group',
      'from': {
        'data': 'fields',
        'transform': [{'type': 'cross'}]
      },
      'properties': {
        'enter': { // each subplot of the splom
          'x': {'scale': 'gx', 'field': 'a.data'},
          'y': {'scale': 'gy', 'field': 'b.data'},
          "width": {"scale": "gx", "band": true, "offset":-35},
          "height": {"scale": "gy", "band": true, "offset":-35},
          "fill": {"value": "#fff"},
          "stroke": {"value": "#ddd"}
        }
      },
      'scales': [{
        'name': 'x',
        'range': 'width',
        'round': true,
        'domain': {'data': 'points', 'field': {'parent': 'a.data'}}
      }, {
        'name': 'y',
        'range': 'height',
        'round': true,
        'domain': {'data': 'points', 'field': {'parent': 'b.data'}}
      }],
      'axes': [
        {'type': 'x', 'scale': 'x'},
        {'type': 'y', 'scale': 'y'}
      ],
      'marks': [{
        'type': 'symbol',
        'from': {'data': 'points'},
        'properties': {
          'enter': {
            'x': {'scale': 'x', 'field': {'datum': {'parent': 'a.data'}}},
            'y': {'scale': 'y', 'field': {'datum': {'parent': 'b.data'}}},
            'fill': {'value': 'darkgrey'},
            'fillOpacity': {'value': 1},
            'size': {'value': 35}
          }
        }
      }]
    }]
  }
};

// from https://gist.github.com/pbeshai/5f0a4d2cd8bb0bc9d96e
var VegaSplom = React.createClass({
  getInitialState: function() {
    // use PureRenderMixin to limit updates when they are not necessary
    this.shouldComponentUpdate = PureRenderMixin.shouldComponentUpdate.bind(this);

    return {vis: null};
  },

  // On initial load, generate the initial vis and attach signal listeners
  componentDidMount: function() {
    var data = this.props.data;
    var fields = this.props.fields;
    var spec = _spec();
    var self = this;

    // parse the vega spec and create the vis
    vg.parse.spec(spec, function(error, chart) {
      var vis = chart({ el: self.refs.chartContainer, renderer: 'canvas' });

      // set the initial data
      vis.data('fields').insert(fields);
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
    var fields = this.props.fields;

    if (vis) {
      // update data in case it changed
      // FIXME: for some reason removing fields breaks things
      //vis.data('fields').remove(function() {return true;});
      vis.data('points').remove(function() {return true;});
      //vis.data('fields').insert(fields);
      vis.data('points').insert(data);

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

exports.fromReact = Pux.fromReact(VegaSplom);
