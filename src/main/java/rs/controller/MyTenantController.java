package rs.controller;

import lombok.extern.log4j.Log4j2;
import org.folio.tenant.rest.resource.TenantApi;

// an example how to override tenant controller
// tne name of the bean MUST be folioTenantController see org.folio.spring.controller.TenantController for details
@Log4j2
//@RestController("folioTenantController")
//@RequestMapping(value = "/_/")
public class MyTenantController implements TenantApi {

//  private final FolioSpringLiquibase folioSpringLiquibase;

//  private final FolioExecutionContextService contextService;

//  @Autowired
//  public MyTenantController(FolioSpringLiquibase folioSpringLiquibase,
//                            FolioExecutionContextService contextService) {
//    this.folioSpringLiquibase = folioSpringLiquibase;
//    this.contextService = contextService;
//  }

//  @Override
//  public ResponseEntity<String> postTenant(@Valid TenantAttributes tenantAttributes) {
//    return null;
//  }
}
