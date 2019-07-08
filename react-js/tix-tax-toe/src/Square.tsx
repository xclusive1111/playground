import React from 'react';

interface SquareProps {
  value: string;
  onClick: () => void;
}

const Square = (props: SquareProps) => {
  return (
    <div className="square" onClick={props.onClick}>
      {props.value}
    </div>
  );
};

export default Square;