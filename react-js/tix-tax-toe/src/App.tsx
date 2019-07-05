import React from 'react';
import './App.css';
import Board from './Board';

class App extends React.Component {
  constructor(props: any) {
    super(props);
    this.state = this.initState();
  }

  initState = () => {
    return {
      squares: Array(9).fill(null),
      nextPlayer: 'X',
    };
  };

  handleClick = (state: any) => (i: number): void => {
    const squares = state.squares.slice();
    const value = squares[i];
    if (value === null) {
      squares[i] = state.nextPlayer;
      this.setState({
        squares: squares,
        nextPlayer: state.nextPlayer === 'X' ? 'O' : 'X'
      });
    }
  };

  render() {
    const state = this.state as any;
    return (
      <div className="flex row space-evenly">
        <div className="flex column center aligned">
          <Board
            squares={state.squares}
            nextPlayer={state.nextPlayer}
            onClick={this.handleClick(this.state)}
          />
          <button onClick={() => this.setState(this.initState())}>Reset</button>
        </div>
      </div>
    );
  }
}

export default App;
