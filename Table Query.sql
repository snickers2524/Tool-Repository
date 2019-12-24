SELECT 
    t3.tool_ID,
    t3.tool_manufacturer,
    t3.tool_model,
    t3.tool_name,
    t3.tool_owner,
    t4.tool_user,
    t4.tool_location,
    t4.tool_notes
FROM
    tools AS t3
        CROSS JOIN
    (SELECT 
        t1.*, t2.tool_notes
    FROM
        (SELECT 
        tc.tool_location, tc.tool_user, tc.tool_date, tc.tool_ID
    FROM
        tools_current AS tc
    INNER JOIN (SELECT 
        MAX(tool_date) AS tool_date, tool_ID
    FROM
        tools_current
    GROUP BY tool_ID) AS t ON tc.tool_date = t.tool_date
        AND tc.tool_ID = t.tool_ID) AS t1
    INNER JOIN (SELECT 
        GROUP_CONCAT(tool_notes
                SEPARATOR ' - ') AS tool_notes,
            tool_ID
    FROM
        tools_current
    GROUP BY tool_ID) AS t2 ON t1.tool_ID = t2.tool_ID) AS t4 ON t3.tool_ID = t4.tool_ID;
        
