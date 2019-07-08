import React from 'react';
import Square from './Square';
import { findFirst } from './collection';

interface BoardProps {
  squares: Array<string>;
  nextPlayer: string;
  onClick: (_: number) => void;
}

const calculateWinner = (squares: Array<string>): string | null => {
  const lines: Array<Array<number>> = [
    [0, 1, 2],
    [3, 4, 5],
    [6, 7, 8],
    [0, 3, 6],
    [1, 4, 7],
    [2, 5, 8],
    [0, 4, 8],
    [2, 4, 6],
  ];

  const isWinner = (line: Array<number>): Boolean => {
    const [a, b, c] = line;
    return squares[a] === squares[b] && squares[a] === squares[c];
  };
  const winner = findFirst(lines, isWinner);
  return winner !== null ? squares[winner[0]] : null;
};


const Board = (props: BoardProps) => {

  const winner = calculateWinner(props.squares);
  const status = winner
    ? 'Winner: ' + winner
    : 'Next player: ' + props.nextPlayer;

  const clickHandler = winner
    ? (_: number) => alert('Winner has already been declared!')
    : props.onClick;

  const renderSquare = (i: number, clickHandler: (_: number) => void) => {
    return <Square
      value={props.squares[i]}
      onClick={() => clickHandler(i)}
    />;
  };

  return (
    <div className="flex column center aligned">
      <div className="flex row mar20">{status}</div>
      <div className="flex row">
        {renderSquare(0, clickHandler)}
        {renderSquare(1, clickHandler)}
        {renderSquare(2, clickHandler)}
      </div>
      <div className="flex row">
        {renderSquare(3, clickHandler)}
        {renderSquare(4, clickHandler)}
        {renderSquare(5, clickHandler)}
      </div>
      <div className="flex row">
        {renderSquare(6, clickHandler)}
        {renderSquare(7, clickHandler)}
        {renderSquare(8, clickHandler)}
      </div>
    </div>
  );
};

export default Board;