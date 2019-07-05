import React from 'react';
import Square from './Square';

class Board extends React.Component {
  props: any;

  constructor(props: any) {
    super(props);
    this.props = props;
  }

  renderSquare(i: number, clickHandler: (_: number) => void) {
    // @ts-ignore
    return <Square
      value={this.props.squares[i]}
      onClick={() => clickHandler(i)}
    />;
  }

  render() {
    const squares = this.props.squares;
    const winner = this.calculateWinner(squares);
    const status = winner
      ? 'Winner: ' + winner
      : 'Next player: ' + this.props.nextPlayer;

    const clickHandler = winner
      ? (_: number) => alert('Winner has already been declared!')
      : this.props.onClick;

    return (
      <div className="flex column center aligned">
        <div className="flex row mar20">{status}</div>
        <div className="flex row">
          {this.renderSquare(0, clickHandler)}
          {this.renderSquare(1, clickHandler)}
          {this.renderSquare(2, clickHandler)}
        </div>
        <div className="flex row">
          {this.renderSquare(3, clickHandler)}
          {this.renderSquare(4, clickHandler)}
          {this.renderSquare(5, clickHandler)}
        </div>
        <div className="flex row">
          {this.renderSquare(6, clickHandler)}
          {this.renderSquare(7, clickHandler)}
          {this.renderSquare(8, clickHandler)}
        </div>
      </div>
    );
  }

  calculateWinner(squares: Array<string>): string | null {
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
    const winner = this.findFirst(lines, isWinner);
    return winner !== null ? squares[winner[0]] : null;
  }

  findFirst<A>(arr: Array<A>, pred: (_: A) => Boolean): A | null {
    for (let i = 0; i < arr.length; i++) {
      if (pred(arr[i])) {
        return arr[i];
      }
    }
    return null;
  }
}

export default Board;