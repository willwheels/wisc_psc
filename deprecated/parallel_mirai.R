##############

# mirai::daemons(4)

# Start the clock!
# ptm <- proc.time()
# 
# test_map_parallel <- purrr::map(1:1000,
#                                 purrr::in_parallel(\(x) process_one_entry(all_pages[start_rows[x]:end_rows[x], 1]),
#                                                    process_one_entry = process_one_entry, 
#                                                    #all_pages = all_pages,
#                                                    start_rows = start_rows, end_rows = end_rows)
#                                 )
# 
# # Stop the clock
# proc.time() - ptm
# 
# 
# 
# test_map_parallel <- bind_rows(test_map_parallel)


#mirai::daemons(0)
## This failed due to memory error--would need to rewrite in order to move less to each daemon
