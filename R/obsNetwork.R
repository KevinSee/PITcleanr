#' @title Observation Network
#'
#' @description Create data.frame describing the observation network and paths from a parent/child dataset.
#'
#' @author Ryan N. Kinzer
#'
#' @param parentchild a dataframe containing at least two columns; 1: parent and 2: child for each observation point in the network.
#'
#' @import dplyr tidygraph
#' @export
#' @return NULL
#' @examples obsNetwork()

obsNetwork <- function(parentchild){

  stopifnot(!is.null(parentchild))

nodes <- parentchild %>%
  gather('loc_type','SiteID', parent, child) %>%
  distinct(SiteID) %>%
  rowid_to_column('id')

edges <- parentchild %>%
  left_join(nodes, by = c('parent' = 'SiteID')) %>%
  rename(from = id) %>%
  left_join(nodes, by = c('child' = 'SiteID')) %>%
  rename(to = id)

routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

path_df <- as_tibble(routes_tidy %>%
                   activate(nodes) %>%
                   mutate(pathIDs = map_dfs(node_is_root(), .f = function(node, path, ...){
                    if(nrow(path) == 0) list(1) else list(path$node)
                     })))

path_df <- path_df %>% unnest(pathIDs) %>%
  unnest(pathIDs) %>%
  left_join(select(nodes, id, pSites = SiteID),
                   by = c('pathIDs' = 'id'))

path_df <- path_df %>%
  group_by(SiteID) %>%
  summarise(path = toString(pSites)) %>%
  mutate(path = paste0(path, ', ', SiteID)) #sep = ", "

network_descrip = stringr::str_split(path_df$path, ', ', simplify = T)

colnames(network_descrip) = paste0('Step', 1:ncol(network_descrip))

site_df = path_df %>%
  bind_cols(network_descrip %>%
              as_tibble()) %>%
  mutate_at(vars(matches('^Step')), funs(as.character)) %>%
  arrange(SiteID)

return(site_df)

#---------------------------------------------------------
# For graphing
#---------------------------------------------------------
# ggraph(routes_tidy, layout = 'tree', circular = TRUE) +
#   geom_edge_link() +
#   geom_node_point() +
#   geom_node_text(aes(label = SiteID)) + #, repel = TRUE
#   theme_graph()
}
