import React, { useEffect, useState } from 'react';
import './App.css';
import Board from './Board';

const App = () => {
  const initialState = {
    squares: Array(9).fill(null),
    nextPlayer: 'X',
  };

  const [state, setState] = useState(initialState);

  useEffect(() => {
    console.log(state);
  }, [state]);

  const handleClick = (state: any) => (i: number): void => {
    const squares = state.squares.slice();
    const value = squares[i];
    if (value === null) {
      squares[i] = state.nextPlayer;
      setState({
        squares: squares,
        nextPlayer: state.nextPlayer === 'X' ? 'O' : 'X'
      });
    }
  };

  return (
    <div className="flex row space-evenly">
      <div className="flex column center aligned">
        <Board
          squares={state.squares}
          nextPlayer={state.nextPlayer}
          onClick={handleClick(state)}
        />
        <button onClick={() => setState(initialState)}>Reset</button>
      </div>
    </div>
  );
};

export default App;
