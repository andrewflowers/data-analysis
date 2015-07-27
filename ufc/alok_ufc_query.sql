select t.event_id, t.start_dt, t.event_desc, t.league_id, t.status_id, s.description,
       m.mma_event_id, f.*,
       (p1.first_nm || ' ' || p1.last_nm) fighter1_name,
       (p2.first_nm || ' ' || p2.last_nm) fighter2_name
      
from sdroltp.event t
left join sdroltp.game_status s on t.status_id = s.game_status_id
left join datagroup.mma_event_mapping m on t.event_id = m.event_id
 
left join datagroup.fights f on m.mma_event_id = f.event_id
left join sdroltp.person p1 on f.fighter_1_id = p1.person_id
left join sdroltp.person p2 on f.fighter_2_id = p2.person_id
 
where t.sport_id = 60 and t.league_id = 3321
 
order by t.start_dt, m.mma_event_id, f.match_number