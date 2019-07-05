import React from 'react';

const Square: React.FC = (props: any) => {
  return (
    <div className="square" onClick={props.onClick}>
      {props.value}
    </div>
  );
};

export default Square;