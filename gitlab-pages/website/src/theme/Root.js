import React, { useState } from 'react';
import SyntaxContext from '@theme/Syntax/SyntaxContext';


let defaultSyntax = 'jsligo';
if (typeof window !== "undefined" && 'localStorage' in window) {
  defaultSyntax = localStorage.getItem('syntax') || defaultSyntax
}

// Default implementation, that you can customize
export default function Root({children}) {
    const [syntax, setSyntax] = useState(defaultSyntax);

    return (
        <SyntaxContext.Provider value={{syntax, setSyntax}}>
            {children}
        </SyntaxContext.Provider>
    );
}
 