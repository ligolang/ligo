import styled from 'styled-components';

export const Group = styled.div`
  display: flex;
  flex-direction: column;
`;

export const HGroup = styled.div`
  display: flex;
  align-items: center;
`;

export const Label = styled.label`
  font-size: 1em;
  color: var(--label_foreground);
  user-select: none;
`;

export const Input = styled.input`
  margin: 0.3em 0 0.7em 0;
  background-color: var(--input_background);
  border-style: none;
  border-bottom: 5px solid #e1f1ff;
  padding: 0.5em;
  font-size: 1em;
  font-family: Menlo, Monaco, 'Courier New', monospace;
  outline: none;

  &:focus {
    background-color: #e1f1ff;
  }
`;

export const Textarea = styled.textarea`
  resize: vertical;
  margin: 0.3em 0 0.7em 0;
  background-color: var(--input_background);
  border-style: none;
  border-bottom: 5px solid #e1f1ff;
  padding: 0.5em;
  font-size: 1em;
  font-family: Menlo, Monaco, 'Courier New', monospace;
  outline: none;

  &:focus {
    background-color: #e1f1ff;
  }
`;
