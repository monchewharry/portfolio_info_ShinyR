(function () {

    // 插入的 iFrame 父节点 
    var parentId = 'js-wscn-hs-wrapper';
    // iFrame 的 ID 
    var iframeId = 'js-wscn-hs-iframe';

    var options = /*options*/ {
        "width": 578,
        "height": 300,
        "symbol": "XAUUSD",
        "interval": "5",
        "chartTheme": "light",
        "type": "area"
    } /*endOptions*/
    
    var hs = new WallstreetCN.embed.Hs(parentId, options, iframeId);
    hs.render();

})();