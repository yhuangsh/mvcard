
-record(qbc, {city, dummy}).
-record(qbs, {id, uid, city}).			% id = base64-encoded uid for now
-record(qbu, {id, pwd, pts}).
-record(qbm, {id, name, addr, tel, rebate}).
-record(qbt, {id, state, ctime, mtime, etime, uid, mid, amt0, pid, promo_discnt, pts_used, amt_actual, pts_earned}).
-record(qbp, {id, value, desc, etime}).		% id = {mid, uid} with uid may be set to undefined, in which case this promo applies to everyone
