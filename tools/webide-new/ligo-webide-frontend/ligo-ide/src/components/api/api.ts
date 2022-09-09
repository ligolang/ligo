import axios, { AxiosInstance, AxiosRequestConfig } from "axios";
import LigoIdeApi from "./LigoIdeApi";

const axiosConfig: AxiosRequestConfig = {
  baseURL: "/api",
};

const axiosInstance: AxiosInstance = axios.create(axiosConfig);

const Api = LigoIdeApi(axiosInstance);

export default Api;
