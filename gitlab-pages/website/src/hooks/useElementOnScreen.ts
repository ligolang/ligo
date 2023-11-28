import { useCallback, useEffect, useRef, useState } from "react";

export default function useElementOnScreen(options) {
  const containerRef = useRef();
  const [isVisible, setIsVisible] = useState(false);

  const intersectionCallback = useCallback((entries) => {
    const [entry] = entries;

    if (entry.intersectionRatio > 0.6) {
      setIsVisible(true);
    } else {
      setIsVisible(false);
    }
  }, []);

  useEffect(() => {
    const observer = new IntersectionObserver(intersectionCallback, options);
    if (containerRef.current) observer.observe(containerRef.current);
    return () => {
      if (containerRef.current) observer.unobserve(containerRef.current);
    };
  }, [containerRef, intersectionCallback]);

  return [containerRef, isVisible];
}
