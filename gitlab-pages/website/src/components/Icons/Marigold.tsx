import { IconType } from "@site/src/components/Icons/Icon.type";
import React from "react";

const Marigold = ({ width, height, color = "currentColor" }: IconType) => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    width={width}
    height={height}
    viewBox="0 0 25 25"
    fill="none"
  >
    <path
      d="M13.324 24.8466C13.8828 24.6065 14.3937 24.2521 14.8525 23.8171L14.8532 23.8163L14.886 23.7855L14.8894 23.7821L14.9181 23.7544L14.9249 23.7469L14.9509 23.7221L14.9613 23.7119L15.0323 23.6403L15.0475 23.6246L15.0673 23.6045L15.0796 23.5916L15.1021 23.568L15.111 23.559C15.5279 23.1187 15.8984 22.6126 16.2234 22.07L16.4449 22.0578C16.6397 22.0469 16.8247 22.0071 16.9979 21.9408C16.8673 22.1955 16.7229 22.4425 16.5667 22.684C16.3922 22.9539 16.6666 23.2886 16.9633 23.1674L20.7357 21.6252C21.2158 21.429 21.5575 21.0836 21.7505 20.6004C21.9779 20.034 22.0887 19.4203 22.108 18.7864V18.785L22.1087 18.7411V18.7355L22.11 18.6949V18.6852V18.6495L22.1107 18.6345L22.11 18.5333V18.5119V18.4837L22.1092 18.4651L22.1087 18.4328V18.4197C22.0941 17.8126 21.9999 17.1917 21.8481 16.5773L21.9965 16.4105C22.1273 16.2631 22.2299 16.1051 22.3055 15.9376C22.392 16.2099 22.4635 16.4865 22.5229 16.7675C22.5894 17.0822 23.0189 17.124 23.1433 16.8275L24.726 13.0561C24.9265 12.5756 24.9258 12.0891 24.7231 11.6097C24.4842 11.0479 24.1316 10.5347 23.6989 10.0736L23.6982 10.0729L23.6672 10.0399L23.6638 10.0365L23.6366 10.0074L23.6289 10.0006L23.6043 9.97446L23.5941 9.96426L23.5228 9.89264L23.5071 9.87759L23.4874 9.8577L23.4743 9.84533L23.4508 9.82245L23.4421 9.81349C23.0041 9.39466 22.5004 9.02231 21.9604 8.69562L21.9481 8.47291C21.9373 8.27566 21.8989 8.09088 21.8345 7.91867C22.087 8.04966 22.332 8.19425 22.5714 8.35058C22.8399 8.52589 23.1729 8.25021 23.0524 7.95206L21.5179 4.16067C21.3226 3.67826 20.9789 3.33508 20.4982 3.14081C19.9349 2.91233 19.324 2.80093 18.6932 2.78176H18.692L18.648 2.78104H18.6427L18.6021 2.7796H18.5926H18.5568L18.5419 2.77887L18.4412 2.7796H18.4199H18.3919L18.3736 2.78032L18.3412 2.78104H18.3282C17.7241 2.79557 17.1061 2.89018 16.4946 3.04281L16.3291 2.89389C16.1825 2.76228 16.0251 2.65892 15.8581 2.58286C16.1289 2.49598 16.4039 2.42405 16.6833 2.36448C16.9966 2.2977 17.0381 1.86599 16.7431 1.74088L12.9913 0.150699C12.5134 -0.051088 12.0294 -0.0502635 11.552 0.153378C10.9933 0.393503 10.4831 0.747918 10.0236 1.18303L10.0229 1.18375L9.98995 1.21456L9.98656 1.21796L9.95785 1.24569L9.95087 1.25321L9.92503 1.27794L9.91468 1.28835L9.8434 1.35977L9.82843 1.37585L9.80854 1.39564L9.79624 1.40872L9.77347 1.43201L9.76455 1.4417C9.3478 1.88145 8.9773 2.38736 8.65233 2.92985L8.43083 2.94222C8.236 2.95304 8.0509 2.99303 7.8777 3.05919C8.00835 2.80443 8.15283 2.5574 8.30901 2.31584C8.48354 2.04593 8.20913 1.7113 7.91247 1.8325L4.14 3.37465C3.65999 3.57087 3.3182 3.91632 3.12521 4.39946C2.89786 4.96586 2.78681 5.57957 2.76773 6.21348V6.21492L2.76701 6.25882V6.26439L2.76558 6.30489V6.31458V6.35023L2.76507 6.36549L2.76558 6.46648V6.48782V6.51595L2.7663 6.53461L2.76701 6.56686V6.58005C2.78147 7.18717 2.87561 7.80819 3.02758 8.42262L2.8792 8.58927C2.74856 8.73664 2.6458 8.89463 2.57023 9.0622C2.48368 8.78992 2.4122 8.51342 2.35283 8.23238C2.28638 7.91754 1.85682 7.8759 1.73243 8.1724L0.149835 11.9437C-0.0506426 12.4241 -0.0501298 12.9107 0.152501 13.3904C0.391638 13.9519 0.744294 14.4652 1.17704 14.9264L1.17776 14.9269L1.20842 14.96L1.21201 14.9636L1.23928 14.9925L1.24677 14.9993L1.27138 15.0255L1.28174 15.0357L1.35301 15.1074L1.3687 15.1224L1.38849 15.1426L1.40151 15.155L1.42469 15.1775L1.43361 15.1865C1.87158 15.6053 2.37519 15.9777 2.91509 16.3043L2.9274 16.5271C2.93827 16.7243 2.97672 16.9091 3.04112 17.0812C2.78875 16.9502 2.54377 16.8057 2.30443 16.6494C2.03596 16.4741 1.70289 16.7498 1.82349 17.0478L3.35778 20.8392C3.55303 21.3216 3.89676 21.6649 4.3775 21.8591C4.94109 22.0876 5.55196 22.199 6.18251 22.2181H6.18395L6.22763 22.2188H6.23317L6.27358 22.2203H6.28321H6.3187L6.33387 22.221L6.43437 22.2203H6.45559H6.4839L6.50225 22.2196L6.53435 22.2188H6.54737C7.15147 22.2043 7.76952 22.1097 8.381 21.957L8.54661 22.1059C8.69366 22.2378 8.85178 22.3401 9.01904 22.4162C8.7476 22.5034 8.47185 22.5754 8.1917 22.6351C7.87842 22.7019 7.83689 23.1336 8.13191 23.2586L11.8844 24.8491C12.3627 25.0511 12.8467 25.0504 13.324 24.8466ZM18.3324 17.3955L18.3319 17.434L18.3321 17.4549C18.3321 17.4578 18.3315 17.4719 18.3315 17.4749L18.3314 17.4938C18.3214 18.2451 18.2194 18.9334 18.031 19.5319C18.0305 19.5335 18.0266 19.545 18.0261 19.5465L17.9769 19.6942C17.975 19.6993 17.9729 19.7043 17.9713 19.7095L17.9372 19.8053C17.8192 20.1184 17.6737 20.4 17.5069 20.653C17.4231 20.78 17.3356 20.902 17.2402 21.0126C17.0611 21.2208 16.8482 21.347 16.5948 21.3979C16.5354 21.4098 16.4743 21.4189 16.4102 21.4225L15.1542 21.4924C14.8581 21.51 14.5922 21.3643 14.4468 21.1052C14.2974 20.8392 14.3116 20.528 14.4862 20.2709L14.4959 20.2563C14.5001 20.2504 14.5042 20.2445 14.508 20.2384L14.5821 20.1263C14.585 20.1218 14.588 20.1172 14.5907 20.1127L14.6448 20.0239C14.6454 20.023 14.6512 20.0138 14.6516 20.0128L14.6757 19.9716C14.6773 19.9689 14.6839 19.9585 14.6854 19.9558L14.7009 19.9293C14.7047 19.9235 14.7082 19.9176 14.7117 19.9115L14.7364 19.8671C15.0825 19.2523 15.3227 18.5671 15.4721 17.7645C15.4834 17.7022 15.4944 17.6394 15.5028 17.5854L15.5197 17.4753C15.5212 17.4685 15.5225 17.4616 15.5234 17.4546C15.5265 17.4342 15.5288 17.4127 15.5317 17.392C15.5851 17.015 15.6195 16.6108 15.6331 16.1876C15.6483 15.7057 16.0153 15.3174 16.4934 15.2837L17.1335 15.2386C17.1464 15.2378 17.1589 15.2381 17.1715 15.2379C17.3564 15.2346 17.5208 15.3 17.6727 15.4398C18.1085 15.8413 18.3234 16.7388 18.3314 17.2105L18.3313 17.2462L18.3319 17.2911C18.3317 17.2964 18.3317 17.3016 18.3317 17.3068L18.3323 17.3892H18.6488L18.3324 17.3955ZM21.4411 13.7022L21.4664 13.7584C21.6042 14.0645 21.7027 14.3696 21.7628 14.6701C21.7922 14.8167 21.8131 14.9624 21.8235 15.1064C21.8428 15.3808 21.7807 15.6213 21.6375 15.8377C21.6039 15.8885 21.5672 15.9385 21.5244 15.9866L20.6859 16.9283C20.4871 17.1516 20.1982 17.237 19.9128 17.1572C19.6201 17.0751 19.4113 16.8448 19.3545 16.543L19.3484 16.5104C19.3476 16.5058 19.3465 16.5013 19.3454 16.4968L19.3396 16.4647C19.3387 16.4595 19.3376 16.4541 19.3365 16.4489L19.3285 16.4124C19.3277 16.4071 19.3267 16.4019 19.3255 16.3966L19.2932 16.2568C19.2925 16.253 19.2895 16.2396 19.2885 16.236L19.263 16.1362C19.2619 16.1318 19.2607 16.1276 19.2595 16.1233L19.247 16.0755C19.0592 15.395 18.7469 14.74 18.2887 14.0683C18.2526 14.0148 18.2155 13.962 18.1807 13.9144L18.1183 13.8288C18.1143 13.8225 18.1101 13.8163 18.1056 13.8103C18.0934 13.7937 18.08 13.7769 18.0676 13.7603C17.8392 13.455 17.5792 13.1447 17.2915 12.8366C16.9635 12.4853 16.9494 11.9502 17.2637 11.5866C17.2675 11.5822 17.2712 11.5776 17.2748 11.5731L17.6848 11.0995C17.6934 11.0895 17.7038 11.0824 17.7128 11.0731C17.8408 10.9395 18.0022 10.8675 18.2077 10.8585C18.7977 10.8321 19.5819 11.3147 19.9232 11.6467L19.9753 11.6977C19.9793 11.7021 19.9835 11.7065 19.9878 11.7108L20.1187 11.8426C20.6399 12.3807 21.0522 12.9399 21.3391 13.4943C21.3405 13.4972 21.3462 13.5092 21.3476 13.5121L21.4318 13.6841C21.4342 13.689 21.4375 13.6956 21.4411 13.7022ZM17.2013 6.57645C17.2077 6.57676 17.2135 6.57655 17.2212 6.57686L17.2801 6.57624L17.301 6.57634C17.304 6.57676 17.3105 6.57614 17.3162 6.57603L17.3468 6.57645L17.37 6.57624L17.3812 6.57655C17.3831 6.57655 17.3852 6.57665 17.3872 6.57665L17.4069 6.57686C18.1545 6.58696 18.8394 6.6894 19.4344 6.87882C19.4367 6.87964 19.449 6.88387 19.4513 6.88459L19.5966 6.93323C19.6015 6.93519 19.6066 6.93715 19.6116 6.9389L19.7096 6.97456C19.7109 6.97507 19.7124 6.97559 19.7138 6.976C20.0226 7.0938 20.3004 7.23911 20.5501 7.40544C20.6765 7.48964 20.7979 7.57755 20.9079 7.6735C21.1153 7.85374 21.2409 8.06769 21.2915 8.32224C21.3034 8.38202 21.3125 8.44334 21.316 8.50775L21.3856 9.76989C21.402 10.069 21.2581 10.3349 21.0003 10.481C20.7355 10.6311 20.4258 10.6167 20.1661 10.4386L20.1553 10.4313C20.149 10.4267 20.1423 10.4221 20.1356 10.418L20.1112 10.4015C20.1076 10.3989 20.1038 10.3963 20.1001 10.3937L20.057 10.3659C20.0557 10.3649 20.0545 10.3642 20.0537 10.3636L20.0244 10.344C20.0216 10.3422 20.0082 10.3338 20.0053 10.3321L19.9302 10.286C19.9277 10.2843 19.9168 10.2772 19.914 10.2756L19.867 10.2475C19.8629 10.2449 19.8585 10.2421 19.8566 10.2411L19.8294 10.225C19.8263 10.223 19.8168 10.2174 19.8136 10.2155L19.7684 10.1899C19.1568 9.84203 18.4751 9.60057 17.6771 9.45062C17.6152 9.43908 17.5521 9.42785 17.4974 9.41929L17.3886 9.40239C17.3819 9.40085 17.3749 9.39961 17.368 9.39858C17.3477 9.39549 17.3263 9.39322 17.3058 9.39033C16.9307 9.33674 16.5284 9.30212 16.1073 9.28851C15.6278 9.27305 15.2414 8.90442 15.2085 8.43076L15.1631 7.78057C15.1622 7.76759 15.1626 7.75491 15.1623 7.74224C15.1587 7.55653 15.224 7.39132 15.3631 7.2388C15.7628 6.8008 16.6557 6.58469 17.1282 6.57665L17.2013 6.57645ZM11.5881 4.97782L11.6418 4.92278C11.6451 4.91969 11.6483 4.9166 11.6515 4.9134L11.7105 4.85384C11.7116 4.8527 11.7155 4.84858 11.7189 4.84497L11.7839 4.78067C12.3193 4.25682 12.8755 3.84264 13.4426 3.54665L13.6167 3.4607C13.6203 3.45906 13.6264 3.45596 13.6322 3.45298L13.6897 3.42659C13.9946 3.28808 14.2982 3.18904 14.5966 3.12855C14.7426 3.09897 14.8876 3.07785 15.0306 3.06733C15.305 3.04827 15.5438 3.11041 15.7589 3.25428C15.8094 3.28808 15.8591 3.32498 15.9071 3.36806L16.8438 4.21076C17.0658 4.41048 17.151 4.7009 17.0716 4.9875C16.9899 5.28173 16.7607 5.49166 16.4588 5.54896L16.4281 5.55484C16.4224 5.55597 16.4167 5.55731 16.4109 5.55875L16.3856 5.56329C16.3778 5.56463 16.37 5.56617 16.3624 5.56803L16.3347 5.57421C16.3274 5.57534 16.3202 5.57668 16.313 5.57833L16.1711 5.61121C16.1653 5.61234 16.1596 5.61368 16.1539 5.61523L16.062 5.63903C16.0567 5.64017 16.0513 5.64161 16.046 5.64305L15.9951 5.65655C15.3171 5.84587 14.6652 6.15999 14.0086 6.61272C14.0068 6.61396 13.9996 6.6187 13.9978 6.61994C13.9447 6.65632 13.8917 6.69393 13.8493 6.72557L13.8366 6.73413C13.8323 6.73701 13.8281 6.74 13.824 6.74309L13.7568 6.79297C13.7515 6.79637 13.7465 6.79998 13.7414 6.80369C13.7249 6.81595 13.7083 6.82945 13.6916 6.84192C13.3884 7.07081 13.0797 7.33217 12.7726 7.62166C12.4228 7.95155 11.8903 7.96525 11.5284 7.64938L11.0441 7.22643C11.0342 7.21777 11.0271 7.20737 11.0178 7.1984C10.8849 7.06968 10.8132 6.90767 10.8043 6.70125C10.7789 6.10753 11.2585 5.32028 11.5881 4.97782ZM10.6292 7.7072L11.1192 8.13488C11.4107 8.38933 11.7712 8.51579 12.1321 8.51579C12.5174 8.51568 12.9031 8.37192 13.2063 8.08604C13.4972 7.8117 13.7886 7.56498 14.0658 7.35546C14.2035 7.47892 14.3616 7.5758 14.5321 7.64217C14.5282 7.70256 14.5268 7.76346 14.5311 7.8253L14.5765 8.47549C14.6323 9.27594 15.2815 9.89892 16.0867 9.92469C16.4819 9.93747 16.8584 9.96962 17.2087 10.0192C17.2191 10.2043 17.2627 10.385 17.3366 10.5531C17.2913 10.593 17.2475 10.6352 17.207 10.682L16.786 11.1684C16.782 11.1728 16.7783 11.1774 16.7748 11.1819C16.258 11.7875 16.281 12.6842 16.8299 13.2718C17.0998 13.5609 17.3435 13.8512 17.5568 14.1358C17.4339 14.2741 17.3375 14.433 17.2714 14.6045C17.2113 14.6005 17.1507 14.5992 17.0893 14.6035L16.4423 14.649C15.646 14.7049 15.026 15.3575 15.0001 16.1668C14.9875 16.564 14.9555 16.9423 14.9061 17.2943C14.722 17.3049 14.5422 17.3487 14.3749 17.4229C14.3352 17.3773 14.2933 17.3333 14.2467 17.2926L13.7624 16.8693C13.7586 16.866 13.7548 16.8628 13.7509 16.8597C13.1487 16.339 12.2552 16.3615 11.6694 16.9134C11.3815 17.1851 11.0927 17.43 10.8099 17.6441C10.6722 17.5206 10.5141 17.4237 10.3434 17.3573C10.3473 17.297 10.3488 17.2361 10.3444 17.1744L10.2992 16.5239C10.2433 15.7236 9.59412 15.1006 8.78893 15.0747C8.39402 15.0619 8.01747 15.0298 7.66677 14.9801C7.65631 14.795 7.61272 14.6143 7.53889 14.4462C7.58422 14.4064 7.628 14.3642 7.66841 14.3174L8.08977 13.831C8.09459 13.8254 8.0992 13.8198 8.10351 13.8139C8.61737 13.2086 8.59347 12.3141 8.04577 11.7272C7.77526 11.4376 7.53171 11.1474 7.31903 10.8633C7.44178 10.7251 7.53807 10.5663 7.60411 10.3951C7.63293 10.3971 7.66164 10.3993 7.69086 10.3993C7.72234 10.3993 7.75434 10.3981 7.78633 10.3959L8.4334 10.3502C9.22967 10.2941 9.84966 9.64169 9.8755 8.83248C9.88812 8.43561 9.92011 8.05718 9.96943 7.70472C10.1536 7.69431 10.3335 7.65041 10.5007 7.57621C10.5406 7.62238 10.5826 7.66649 10.6292 7.7072ZM6.54327 7.60445L6.54378 7.56714L6.54368 7.54148C6.54378 7.53973 6.54409 7.52684 6.54409 7.52509L6.54419 7.50623C6.55424 6.75463 6.65628 6.06652 6.84517 5.46744C6.84589 5.46528 6.84999 5.45291 6.8507 5.45075L6.89623 5.31244C6.89746 5.30904 6.90321 5.29389 6.90444 5.29049L6.93889 5.19465C7.05641 4.88197 7.20182 4.60052 7.36866 4.34762C7.45255 4.22045 7.54012 4.09832 7.6358 3.98754C7.81494 3.77926 8.02783 3.65311 8.28132 3.6022C8.3407 3.59025 8.40171 3.58118 8.4658 3.57757L9.72158 3.5077C10.0181 3.49028 10.2836 3.6358 10.429 3.89489C10.5785 4.16088 10.5641 4.47211 10.3902 4.72842L10.3795 4.74429C10.3752 4.75016 10.3712 4.75614 10.3674 4.76222L10.3494 4.78912C10.3468 4.79262 10.3444 4.79613 10.3421 4.79973L10.3138 4.84394C10.3132 4.84497 10.3125 4.84601 10.312 4.84662L10.2885 4.88259C10.288 4.88311 10.2812 4.89434 10.2808 4.89486L10.2533 4.93917C10.251 4.94267 10.2483 4.94772 10.2455 4.95277L10.2125 5.00636C10.2101 5.01018 10.2078 5.01399 10.2057 5.01791L10.1981 5.03141C10.195 5.03604 10.1922 5.04068 10.1895 5.04532L10.1727 5.07438C10.1695 5.07933 10.1665 5.08448 10.1637 5.08963L10.1395 5.13302C9.79357 5.74724 9.5534 6.43227 9.404 7.23478C9.39251 7.29692 9.38164 7.35958 9.3717 7.4207L9.35611 7.52344C9.35447 7.53076 9.35324 7.53797 9.35211 7.54529C9.34903 7.5657 9.34678 7.58713 9.3439 7.60775C9.29068 7.98525 9.25612 8.38954 9.24269 8.81239C9.22741 9.29418 8.8604 9.6826 8.3892 9.71579L7.74214 9.76134C7.72921 9.76227 7.7167 9.76196 7.70409 9.76217C7.5193 9.76608 7.35492 9.70002 7.20305 9.56017C6.76723 9.15856 6.55229 8.26113 6.5443 7.78789L6.54378 7.7074C6.54399 7.70266 6.54399 7.69782 6.54399 7.69308L6.54348 7.61063H6.22692L6.54327 7.60445ZM3.43602 11.3002L3.42649 11.2793C3.42577 11.2775 3.42454 11.2749 3.42331 11.2724L3.41069 11.2438C3.40926 11.2406 3.40772 11.2374 3.40629 11.2343C3.2701 10.9305 3.17258 10.6276 3.1129 10.3294C3.08368 10.1831 3.06276 10.0377 3.0523 9.89418C3.03302 9.61984 3.09506 9.37889 3.23821 9.16237C3.27185 9.11146 3.30856 9.06158 3.35132 9.01335L4.19004 8.07181C4.38898 7.84849 4.67775 7.76305 4.96314 7.84292C5.25591 7.92496 5.46469 8.15529 5.5214 8.45663L5.52755 8.48971C5.52827 8.49342 5.52909 8.49703 5.52991 8.50064L5.53627 8.53516C5.53719 8.5397 5.53801 8.54423 5.53904 8.54876L5.54755 8.58762C5.54837 8.59287 5.5494 8.59803 5.55052 8.60328L5.58293 8.74478C5.58354 8.74756 5.58631 8.75993 5.58703 8.76282L5.61051 8.85412C5.61226 8.86258 5.61431 8.87082 5.61687 8.87906L5.62897 8.92462C5.81673 9.60511 6.12898 10.2602 6.58367 10.9273C6.58562 10.9302 6.5887 10.9344 6.59198 10.9387C6.62664 10.9899 6.66202 11.0404 6.69524 11.0857L6.75841 11.1725C6.76221 11.1784 6.7661 11.1841 6.77031 11.1897C6.78261 11.2063 6.79605 11.2232 6.80856 11.2399C7.03621 11.5444 7.29616 11.8548 7.58432 12.1633C7.91226 12.5147 7.92621 13.0499 7.61088 13.4147C7.60677 13.4195 7.60288 13.4244 7.59898 13.4294L7.19085 13.9004C7.18223 13.9104 7.17188 13.9176 7.16285 13.9269C7.03477 14.0604 6.87347 14.1325 6.66807 14.1415C6.07381 14.1653 5.29385 13.6853 4.95196 13.3525L4.89782 13.2995C4.89484 13.2963 4.89177 13.2931 4.88869 13.29L4.75713 13.1574C4.23619 12.6197 3.82396 12.0605 3.53467 11.5011C3.53416 11.5001 3.52924 11.4899 3.52873 11.489L3.50247 11.4365C3.50114 11.4338 3.49971 11.4311 3.49837 11.4283L3.44382 11.3158C3.44197 11.3119 3.439 11.306 3.43602 11.3002ZM7.67118 18.4236C7.66636 18.4234 7.66194 18.423 7.65692 18.4233L7.62503 18.4234C7.62267 18.4234 7.61734 18.4235 7.61262 18.4237H7.59365L7.56586 18.424L7.52874 18.4235L7.50413 18.4236L7.49316 18.4234C7.49172 18.4234 7.49018 18.4234 7.48875 18.4234L7.46906 18.4232C6.72129 18.4131 6.03649 18.3106 5.44029 18.1208C5.43813 18.12 5.42603 18.1158 5.42398 18.1152L5.27826 18.0666C5.27365 18.0647 5.26893 18.0629 5.26432 18.0613L5.16885 18.0266C4.85741 17.9084 4.57726 17.7622 4.32561 17.5945C4.19927 17.5103 4.07786 17.4224 3.96783 17.3266C3.76058 17.1465 3.63496 16.9324 3.5842 16.6777C3.57231 16.6181 3.56328 16.5568 3.55969 16.4923L3.49017 15.2302C3.47376 14.9311 3.61784 14.6653 3.87554 14.5192C4.14031 14.3689 4.4501 14.3836 4.70308 14.5569L4.71928 14.568C4.72718 14.5738 4.73539 14.5793 4.74379 14.5843L4.78881 14.6155C4.79281 14.6181 4.79681 14.6207 4.80081 14.6233L4.82009 14.6351C4.82081 14.6357 4.82162 14.6362 4.82224 14.6366L4.85588 14.6591C4.85813 14.6605 4.87085 14.6686 4.8731 14.67L4.94571 14.7143C4.94807 14.7158 4.95852 14.7227 4.96109 14.7242L5.00949 14.753C5.01318 14.7554 5.01728 14.758 5.01903 14.7589L5.04918 14.7768C5.05153 14.7782 5.06015 14.7833 5.06251 14.7847L5.10773 14.8104C5.71952 15.1583 6.40114 15.3995 7.19905 15.5495C7.26109 15.561 7.32385 15.5722 7.37912 15.5808L7.48854 15.5978C7.495 15.5992 7.50136 15.6004 7.50792 15.6013C7.52822 15.6044 7.54955 15.6067 7.57017 15.6096C7.94589 15.6632 8.34818 15.6979 8.76893 15.7114C9.24823 15.7268 9.63473 16.0956 9.66775 16.569L9.71297 17.2195C9.71389 17.2324 9.71348 17.245 9.71379 17.2578C9.71738 17.4435 9.65216 17.6087 9.5129 17.7612C9.11328 18.1992 8.22041 18.4152 7.74798 18.4232L7.67118 18.4236ZM11.4438 21.4479C11.4422 21.4486 11.4316 21.4538 11.4301 21.4547L11.2602 21.5387C11.2559 21.5406 11.2493 21.5439 11.2431 21.5472L11.2217 21.5571C11.2203 21.5578 11.2181 21.5588 11.2159 21.5599L11.1858 21.5735C10.8814 21.7121 10.5777 21.8111 10.2787 21.8716C10.1329 21.9011 9.9882 21.9221 9.84505 21.9327C9.57228 21.9513 9.33242 21.8894 9.11697 21.7457C9.06631 21.7119 9.01658 21.675 8.96848 21.6319L8.03183 20.7892C7.80971 20.5894 7.7245 20.2991 7.80407 20.0125C7.8857 19.7182 8.11499 19.5082 8.42919 19.4487L8.4456 19.4456C8.45237 19.4443 8.45903 19.4428 8.4658 19.441L8.49031 19.4366C8.49534 19.4358 8.50046 19.4347 8.50549 19.4337L8.54456 19.4252C8.55061 19.4241 8.55676 19.423 8.56281 19.4216L8.70638 19.3883C8.71109 19.3872 8.71591 19.3861 8.72063 19.385L8.81979 19.3593C8.824 19.3583 8.8283 19.3571 8.83251 19.3559L8.88009 19.3433C9.55751 19.1543 10.2094 18.8404 10.8777 18.3801C10.9307 18.3437 10.9833 18.3066 11.0339 18.2694L11.1143 18.2101C11.121 18.2058 11.1276 18.2012 11.1342 18.1964C11.1506 18.1841 11.1673 18.1706 11.1838 18.1582C11.487 17.9291 11.7958 17.6676 12.1028 17.3781C12.4524 17.0485 12.9851 17.0345 13.3469 17.3505C13.3506 17.3537 13.3542 17.3567 13.3581 17.3597L13.8312 17.7732C13.8312 17.7734 13.8313 17.7734 13.8313 17.7734C13.8413 17.782 13.8483 17.7924 13.8577 17.8015C13.9905 17.9301 14.0622 18.0922 14.0711 18.2986C14.0967 18.8927 13.6171 19.6798 13.2857 20.0241L13.2328 20.0786C13.23 20.0812 13.2273 20.0838 13.2248 20.0864L13.0919 20.2195C12.5567 20.7431 12.0002 21.1574 11.4438 21.4479Z"
      fill={color}
    />
  </svg>
);

export default Marigold;
