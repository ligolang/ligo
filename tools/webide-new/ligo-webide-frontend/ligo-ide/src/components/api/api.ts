import axios, { AxiosInstance, AxiosRequestConfig } from "axios";
import LigoIdeApi from "./LigoIdeApi";

const axiosConfig: AxiosRequestConfig = {
  baseURL: "https://ligo-web-ide-dev-new.gcp-npr.marigold.dev/",
};

const axiosInstance: AxiosInstance = axios.create(axiosConfig);

const Api = LigoIdeApi(axiosInstance);

export default Api;
