select e.start_dt, e.event_id, e.event_desc,  l.league_abbr, v.name venue, initcap(o.name) city, st.name state, cu.name country, f.fight_card_order, 
       f1.person_id, f1.first_nm||' '||f1.last_nm fighter1, f2.person_id, f2.first_nm||' '||f2.last_nm fighter2, f1.gender fighter1_gender, f2.gender fighter2_gender,
       case when fw.person_id = f1.person_id then f1.person_id else f2.person_id end as winner_id,
       case when fw.person_id = f1.person_id then f1.first_nm||' '||f1.last_nm else f2.first_nm||' '||f2.last_nm end as fight_winner,
       f.rounds_scheduled, f.rounds_fought, f.ending_time, f.total_time, f.result_id, rl.result,
       f.target_id, tl.target, f.position_id, pl.position,
       f.submission_id, sm.submission, f.ending_strike_id, es.description ending_strike
         
       
     from boxmma.fights f 
     left join sdroltp.event e on e.event_id = f.event_id
     left join sdroltp.league l on l.league_id = e.league_id
     left join sdroltp.venue v on v.venue_id = e.venue_id
     left join sdroltp.location o on o.location_id = v.location_id
     left join sdroltp.state st on st.state_id = o.state_id
     left join sdroltp.country cu on cu.country_id = o.country_id
     left join (select distinct fd.fight_id, min(fpc.person_id) fighter_1, max(fpc.person_id) fighter_2
                from boxmma.fight_details fd
                join boxmma.fighter_player_cards fpc on fpc.id = fd.player_card_id
                group by fd.fight_id
                ) fr on fr.fight_id = f.id
     left join sdroltp.person f1 on f1.person_id = fr.fighter_1
     left join sdroltp.person f2 on f2.person_id = fr.fighter_2
     left join (select distinct fd.fight_id, fpc.person_id
                from boxmma.fight_details fd
                join boxmma.fighter_player_cards fpc on fpc.id = fd.player_card_id
                where fd.decision_id = 1  
                ) fw on fw.fight_id = f.id
     left join boxmma.result_lookups rl on rl.id = f.result_id                
     left join boxmma.target_lookups tl on tl.id = f.target_id
     left join boxmma.position_lookups pl on pl.id = f.position_id       
     left join boxmma.submission_type sm on sm.id = f.submission_id   
     left join boxmma.ending_strike es on es.id = f.ending_strike_id
                
                     where e.sport_id = 60  
           --and e.status_id = 10504 -- Final only?
           and e.start_dt between '01-JAN-1990' and sysdate
     
     order by e.start_dt desc, e.event_desc asc ,f.fight_card_order asc