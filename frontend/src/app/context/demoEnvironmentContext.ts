import { createContext } from "react";
import { DemoEnvironment, previewEnv } from "../store/types";

const DemoEnvironmentContext = createContext<DemoEnvironment>(previewEnv);

export default DemoEnvironmentContext;