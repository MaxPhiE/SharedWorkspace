{-# LANGUAGE QuasiQuotes                #-}

module WS_CSS where

import Yesod


{--------------------------------
 - css widget -------------------
 --------------------------------}

defaultCSSL = [lucius| 
    form {
        padding:            20px 20px;
        border-radius:      15px;
        --background-color:   #78C94F;
        background-color:   #83B81A;
        font-size:          14px;
        font-family:        arial;
    }
    input {
        font-size:          14px;
        width:              200px;
    }
    select {
        font-size:          14px;
        width:              208px;
    }
    button {
        margin-top:         20px;
        font-size:          14px;
    }
    textarea {
        font-size:          14px;
        font-family:        arial;
        width:              100%;
        height:             470px;
        margin-top:         10px;
        margin-bottom:      10px
    }
    .titleClass {
        padding:            10px 20px;
        border-radius:      15px;
        --background-color:   darkgreen;
        background-color:   #83B81A;
        color:              white;
        font-size:          32px;
        font-weight:        bold;
    }
    .footerClass {
        padding:            10px 20px;
        border-radius:      15px;
        --background-color:   darkgreen;
        background-color:   #83B81A;
        --color:              white;
        color:              #004149;
        font-size:          14px;
        font-weight:        bold;
        position:           absolute;
        top:                720px;
        width:              96%;
    }
    .subTitleClass {
        padding:            10px 20px;
        border-radius:      15px;
        --background-color:   darkgreen;
        background-color:   #83B81A;
        color:              white;
        font-size:          20px;
        font-weight:        bold;
        margin-top:         15px;
        margin-bottom:      3px;
    }
    body {
        --background-color:   #D2E6C8;
        background-color:   #FFFFFF;
        font-family:        arial;
    }
    button {
        margin-top:         20px;
        margin-left:        216px;
        font-size:          14px;
        font-family:        arial;
    }
    div {
        margin-top:         10px;
    }
    label {
        display:            inline-block;
        width:              70px;
    }
    .longLabel {
        display:            inline-block;
        width:              200px;
    }
    p {
        margin-left:        25px;
    }
    table {
        width:              100%;
    }
    #logoutLink {
        float:              right;
        padding-right:      20px;
    }
    #homeLink, #newUserLink, #newWsLink, #myUserLink {
        float:              right;
        padding-right:      10px;
    }
    #tabTable {
        margin-top:         10px;
        margin-bottom:      10px;
    }
    #assignmentTable {
        margin-left:        15px;
    }
    #workspaceOrderTable {
        margin-left:        15px;
        margin-top:         10px;
        width:              200px;
    }
    .tabClass {
        padding:            10px 10px;
        border-radius:      5px;
        --background-color:   #78C94F;
        background-color:   #83B81A;
        font-weight:        bold;
        --color:              #444;
        color:              #004149;
        font-size:          12px;
        text-decoration:    none;
        width:              140px;
        display:            inline-block;
        text-align:         center;
    }
    .tabClassActive {
        padding:            10px 10px;
        border-radius:      5px;
        --background-color:   darkgreen;
        background-color:   #004149;
        font-weight:        bold;
        color:              white;
        font-size:          12px;
        text-decoration:    none;
        width:              140px;
        display:            inline-block;
        text-align:         center;
    }
    .tabClassArrow {
        padding:            10px 10px;
        border-radius:      5px;
        --background-color:   darkgreen;
        background-color:   #83B81A;
        font-weight:        bold;
        color:              004149;
        font-size:          12px;
        text-decoration:    none;
        display:            inline-block;
        text-align:         center;
        cursor:             pointer;
    }
    .adminBtn {
        float:              right;
        width:              100px;
    }
    .btn {
        width:              100px;
    }
    .btnShort {
        width:              60px;
    }
    .btnLong {
        width:              212px;
    }
    .list {
        list-style-type:    none;
        padding-left:       15px;
        padding-right:      500px;
        margin-top:         50px;
        float:              left;
    }
    ul {
        margin-top:         10px;
        line-height:        1.5;
    }
    #saveUser, #save {
        margin-left:        72px;
    }
    #setPassword {
        margin-left:        202px;
    }
    
    #tabs {
        width: 50%;
        border-style: none;
    }
|]