using DDDNetCore.Domain.ConnectionRequests;
using DDDNetCore.Domain.ConnectionRequests.DTOS;
using Microsoft.AspNetCore.Mvc;
using System.Threading.Tasks;

namespace DDDNetCore.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class ConnectionRequestsController : ControllerBase
    {
        private readonly ConnectionRequestService _service;

        public ConnectionRequestsController(ConnectionRequestService service)
        {
            _service = service;
        }

        // GET: api/ConnectionRequests
        [HttpGet]
        public async Task<ActionResult<ConnectionRequestListDto>> GetAll()
        {
            return await _service.GetAllAsync();
        }

    }
}
